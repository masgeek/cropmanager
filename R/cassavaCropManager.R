#' SHORT DEF:   Wrapper function to calculate recommendations across all use cases.
#' RETURNS:     Vector of with recommendation texts to display.
#' DESCRIPTION: Function with all inputs required by the DST app, and calling use case-specific wrapper functions to calculated recommendations on IC, FR, PP and SP.
#'             This function also calls on functions to send recommendation reports by SMS and/or email.
#' INPUT:
#' @param country    : 2-letter country code
#' @param lat        : Latitude in decimal degrees
#' @param lon        : Longitude in decimal degrees
#' @param area       : Numerical value for area of the field
#' @param areaUnits  : Units for area (acre, ha or m2)
#' @param IC         : Logical, indicating if an intercrop (TRUE) or a monocrop (FALSE) is grown
#' @param intercrop  : Intercrop species grown, either maize or sweetpotato, or NA if monocrop
#' @param FR         : Logical, indicating if fertilizer recommendations are requested, NA if IC == TRUE and country == "TZ"
#' @param PP         : Logical, indicating if planting practice recommendations are requested, NA if IC == TRUE or country == "TZ"
#' @param SPP        : Logical, indicating if scheduled planting - advice on planting date is requested, NA if IC == TRUE
#' @param SPH        : Logical, indicating if scheduled planting - advice on harvest date is requested, NA if IC == TRUE
#' @param PD         : Planting date, in format "yyyy-mm-dd" (%Y-%m-%d)
#' @param HD         : Harvest data, in format "yyyy-mm-dd" (%Y-%m-%d)
#' @param tractor    : Logical, indicating if the user has access to a tractor, NA if PP != TRUE
#' @param implement  : vector containing implements available for tractor (plough, harrow, ridger)
#' @param herbicide  : Logical, indicating if the user has access to herbicides for weed control
#' @param nTill1     : Number of primary tillage operations conducted in current practice (NA, 0, 1, 2), NA if PP != TRUE
#' @param nHarrow    : Number of harrow operations conducted in current practice (NA, 0, 1, 2), NA if PP != TRUE
#' @param ridges     : Logical indicating if the farmer ridges his/her field in current practice (NA, TRUE, FALSE), NA if PP != TRUE
#' @param weedMethod : Vector containing methods used for weed control (NA, manual, herbicide), NA if PP!= TRUE
#' @param costLMO    : Dataframe containing cost of land management operations; costs are provided in local currency, for the area of the field
#' @param costWCO    : Dataframe containing cost of weed management operations; costs are provided in local currency, for the area of the field
#' @param FCY        : Farmer-reported current yield, in tonnes FM per ha (optional, default value = NA)
#' @param CMP        : Current maize performance, score on a scale of 1 (very yellow and stunted) .. 5 (tall and dark green), NA if IC != TRUE and FR != TRUE, or NA if the user does not know (NA = default)
#' @param fertilizers: Dataframe containing available fertilizers and their cost per bag (in local currency) and bag weight (in kg)
#' @param saleSF     : Logical, indicating if the user is selling roots to a registered starch factory at factory-fixed prices
#' @param nameSF     : Name of starch factory where roots will be sold, NA if saleSF = FALSE
#' @param cassPD     : Type of cassava produce sold (roots, chips, flour, gari)
#' @param cassUW     : Unit weight at which cassava produce is sold, in kg; common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag) and 1000 (per tonne); can be NA if user does not know.
#' @param cassUP     : Price of 1 cassava produce unit in local currency; can be NA if user does not know.
#' @param maizePD    : Type of maize produce sold (fresh cobs, dry cobs, grain), NA if IC != TRUE and country == "NG"
#' @param maizePC    : Logical indicating if maize is sold per cob (NA, TRUE, FALSE), NA if maizePD != "fresh_cob"
#' @param maizeUW    : Unit weight at which maize produce is sold, in kg; common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "NG" or NA if maizePD == "grain"; can be NA if user does not know.
#' @param maizeUP    : Price of 1 maize produce unit (or cob, if maizePC == TRUE) in local currency, NA if IC != TRUE and country == "NG"; can be NA if user does not know.
#' @param maxInv     : Maximal investment in fertilizer, for the area of the field in local currency, NA if FR != TRUE, default = NA (if user does not wish to set an investment ceiling)
#' @param SMS        : Logical indicating if recommendations must be sent by SMS to the user
#' @param email      : Logical indicating if recommendations must be sent by email to the user
#' @param userPhoneCC: Country code of the phone number of the user requesting the recommendations (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS); example 234 for Nigeria
#' @param userPhoneNr: Phone number of the user requesting the recommendations, without the initial zero (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS); excludes the initial zero, stored as numerical (e.g., 789123456)
#' @param userName   : Name of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#' @param userEmail  : Email address of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#' @param userField  : Name or desciption of the field (to be included in the email report, and aid the user to recall for which field recommendations were requested), default = NA (if user does not wish to receive recommendations by email)
#' @export
cassavaCropManager <- function(country = c("NG", "TZ"), #select one
                               lat,
                               lon,
                               area,
                               areaUnits = c("acre", "ha", "m2"), #select one
                               IC = c(TRUE, FALSE), #select one
                               intercrop = c(NA, "maize", "sweetpotato"), #select one
                               FR = c(NA, TRUE, FALSE), #select one
                               PP = c(NA, TRUE, FALSE), #select one
                               SPP = c(NA, TRUE, FALSE), #select one
                               SPH = c(NA, TRUE, FALSE), #select one
                               PD,
                               HD,
                               tractor = c(NA, TRUE, FALSE), #select one
                               implement = c("plough", "harrow", "ridger"), #select multiple (or NA if none)
                               herbicide = c(NA, TRUE, FALSE), #select one
                               nTill1 = c(NA, 0, 1, 2), #select one
                               nHarrow = c(NA, 0, 1, 2), #select one
                               ridges = c(NA, TRUE, FALSE), #select one
                               weedMethod = c("manual", "herbicide"), #select multiple (or NA if not indicated)
                               costLMO = expand.grid(operation = c("plough1", "plough2", "harrowing", "ridging"),
                                                     tractor = c(TRUE, FALSE),
                                                     cost = NA),
                               costWCO = expand.grid(operation = c("clearing", "weeding1", "weeding2"),
                                                     herbicide = c(TRUE, FALSE),
                                                     cost = NA),
                               FCY = NA,
                               CMP = NA,
                               fertilizers = expand.grid(type = c("urea", "CAN", "NPK17:17:17", "NPK15:15:15", "NPK20:10:10", "DAP", "TSP", "SSP", "Minjingu_Nafaka+", "MOP"),
                                                         available = FALSE,
                                                         price = NA,
                                                         weight = 50),
                               saleSF = c(NA, TRUE, FALSE),
                               nameSF = c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"),  #select one
                               cassPD = c("roots", "chips", "flour", "gari"), #select one
                               cassUW = NA,
                               cassUP = NA,
                               maizePD = c(NA, "fresh_cob", "dry_cob", "grain"), # select one
                               maizePC = c(NA, TRUE, FALSE), #select one
                               maizeUW = NA,
                               maizeUP = NA,
                               maxInv = NA,
                               SMS = c(TRUE, FALSE),
                               email = c(TRUE, FALSE),
                               userPhoneCC = NA,
                               userPhoneNr = NA,
                               userName = NA,
                               userEmail = NA,
                               userField = NA
                               ){

  # obtain currency and conversion rate from country
  coucun <- data.frame(country =  c("NG", "TZ"),
                       currency = c("NGN", "TZS"),
                       conversion = c(360, 2250))
  currency <- coucun[coucun$country==country,]$currency
  conversion <- coucun[coucun$country==country,]$conversion

  # generate list with requested recommendations
  res <- list()
  if(IC == TRUE)                        {res[["IC"]] <- getICrecommendations(...)}
  if(FR == TRUE & intercrop == FALSE)   {res[["FR"]] <- getFRrecommendations(...)}
  if(PP == TRUE)                        {res[["PP"]] <- getPPrecommendations(...)}
  if(SPP == TRUE | SPH == TRUE)         {res[["SP"]] <- getSPrecommendations(...)}


  # extract the recommendation texts to a vector
  recText <- NULL
  for(i in 1:length(res)){
    recText <- c(recText, res[[i]][["recText"]])
  }

  # send recommendations to user
  if(email == TRUE)                     {sendEmailReport(res, userName, userEmail, userField)}
  if(SMS == TRUE)                       {sendSMSReport(SMStext = recText, dst = paste(userPhoneCC, userPhoneNr, sep=""))}

  return(recText)

}





#SHORT DEF:   Function to send SMS report.
#RETURNS:     Nothing. SMS report are sent.
#             TODO: build in checks to log if SMS report was successfully sent.
#DESCRIPTION: Function using Plivo service to send SMS texts to phonenumber specified.
#             Note: Plivo credentials are hardcoded! Do not share!!!
#             TODO: use scan function to read credentials from csv input file.
#INPUT:       text: Vector of body text to be sent by SMS. Elements should not exceed 1600 character limit!
#             src: source phone number, starting with country code, default 254727876796
#             dst: destination phone number, starting with country code, e.g., 234789123456
#' @export
sendSMSReport <- function(SMStext, src="254727876796", dst){
	if(is.list(res)){
	#plivio account details
		AUTH_ID="MANDM1MDCYNWU4NGEZZW"
		AUTH_TOKEN="M2Q2MmQ0NjI3ZjNjOTBkYjMyNGMzNzUzODdmZTc3"
		url="https://api.plivo.com/v1/Account/MANDM1MDCYNWU4NGEZZW/Message/"

		for(i in SMStext){
			if(nchar(i)<=1600){
				POST(url,
						authenticate(AUTH_ID,AUTH_TOKEN),
						body=list(src=src, dst=dst, text=i))
			}else{
				print("Text message exceeds 1600 character limit. Message not sent")
			}
		}
	}
}

#sendSMSReport <- function(text, src="254702974480", dst){
##sendSMSReport <- function(text, src="254727876796", dst){
#  if(!library("httr")){
#    install.packages("httr")
#    library("httr")
#  }
#
#  #plivio account details
#  AUTH_ID="MANDM1MDCYNWU4NGEZZW"
#  AUTH_TOKEN="M2Q2MmQ0NjI3ZjNjOTBkYjMyNGMzNzUzODdmZTc3"
#  url="https://api.plivo.com/v1/Account/MANDM1MDCYNWU4NGEZZW/Message/"
#
#  for(i in text){
#    if(nchar(i)<=1600){
#      POST(url,
#           authenticate(AUTH_ID,AUTH_TOKEN),
#           body=list(src=src, dst=dst, text=i))
#    }else{
#      print("Text message exceeds 1600 character limit. Message not sent")
#    }
#  }
#}

#' function to send mail from acai.akilimo@gmail.com
#' @export
sendEmailReport <- function(res=res, userName, userEmail, userField){
	if(is.list(res)){
		##knit("fertilizer_advice.Rmd")
		rmarkdown::pandoc_version()
		pp <- "fertilizer_advice.png"
		if (file.exists(pp)) file.remove(pp)

		webshot::rmdshot('fertilizer_advice.Rmd', 'fertilizer_advice.png', delay = 3)

		send.mail(from = "acai.akilimo@gmail.com",
				to = as.character(userEmail) ,
				subject = "ACAI recommendation",
				html = T,
				inline = T,
				body = "rmarkdown output",
				smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "acai.akilimo@gmail.com", passwd = "akilimo101", ssl = T),
				authenticate = T,
				send = TRUE,
				attach.files = "fertilizer_advice.png",
				debug = F)
	}

}

#' function to put data used in the markdown .rmd
#' @export
fertilizerAdviseTable <- function(){
	acairm <- read.csv("MarkDownTextD.csv")
	dat <- subset(acairm, select=c(fertilizer1, bags1, total_cost1,kgs1, currency, field_area))
	if(is.na(dat$bags1) | dat$bags1==0){
		fn <- "datall1.csv"
		if (file.exists(fn)) file.remove(fn)
	}else{
		datall1<-NULL
		for (i in 1:nrow(dat)){
			dat_user1 <- dat[i,]
			if (dat_user1$bags1==0.5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/half.png)')
			}else if (dat_user1$bags1==1){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/1.png)')
			}else if (dat_user1$bags1==1.5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/1_5.png)')
			}else if (dat_user1$bags1==2){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/2.png)')
			}else if (dat_user1$bags1==2.5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/2_5.png)')
			}else if (dat_user1$bags1==3){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/3.png)')
			}else if (dat_user1$bags1==3.5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/3_5.png)')
			}else if (dat_user1$bags1==4){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/4.png)')
			}else if (dat_user1$bags1==4.5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/4_5.png)')
			}else if (dat_user1$bags1==5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/5.png)')
			}else if (dat_user1$bags1==5.5){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/5_5.png)')
			}else if (dat_user1$bags1==6){
				dat_user1$rep1 <- sprintf('![](D:/ACAI_Wrapper/net/green/6.png)')
			}
			datall1<-rbind(datall1,dat_user1)
		}

		write.csv(datall1, "datall1.csv")
	}



	## Loop

	dat2 <- subset(acairm, select=c(fertilizer2, bags2, total_cost2, kgs2, currency, field_area))
	if(is.na(dat2$bags2) | dat2$bags2==0){
		fn <- "datall2.csv"
		if (file.exists(fn)) file.remove(fn)
	}else{
		datall2<-NULL
		for (i in 1:nrow(dat2)){
			dat_user2<- dat2[i,]
			if (dat_user2$bags2==0.5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/half.png)')
			}else if (dat_user2$bags2==1){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/1.png)')
			} else if (dat_user2$bags2==1.5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/1_5.png)')
			}else if (dat_user2$bags2==2){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/2.png)')
			}else if (dat_user2$bags2==2.5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/2_5.png)')
			}else if (dat_user2$bags2==3){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/3.png)')
			}else if (dat_user2$bags2==3.5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/3_5.png)')
			}else if (dat_user2$bags2==4){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/4.png)')
			}else if (dat_user2$bags2==4.5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/4_5.png)')
			}else if (dat_user2$bags2==5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/5.png)')
			}else if (dat_user2$bags2==5.5){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/5_5.png)')
			}else if (dat_user2$bags2==6){
				dat_user2$rep2 <- sprintf('![](D:/ACAI_Wrapper/net/grey/6.png)')
			}
			datall2<-rbind(datall2,dat_user2)
		}
		write.csv(datall2, "datall2.csv")
	}


	## Loop

	dat3 <- subset(acairm, select=c(fertilizer3, bags3, total_cost3,kgs3, currency, field_area))

	if(is.na(dat3$bags3) | dat3$bags3==0){
		fn <- "datall3.csv"
		if (file.exists(fn)) file.remove(fn)
	}else{
		datall3<-NULL
		for (i in 1:nrow(dat3)){
			dat_user1<- dat3[i,]
			if (dat_user1$bags3==0.5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/half.png)')
			}else if (dat_user1$bags3==1){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/1.png)')
			}else if (dat_user1$bags3==1.5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/1_5.png)')
			}else if (dat_user1$bags3==2){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/2.png)')
			}else if (dat_user1$bags3==2.5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/2_5.png)')
			}else if (dat_user1$bags3==3){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/3.png)')
			}else if (dat_user1$bags3==3.5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/3_5.png)')
			}else if (dat_user1$bags3==4){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/4.png)')
			}else if (dat_user1$bags3==4.5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/4_5.png)')
			}else if (dat_user1$bags3==5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/5.png)')
			}else if (dat_user1$bags3==5.5){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/5_5.png)')
			}else if (dat_user1$bags3==6){
				dat_user1$rep3 <- sprintf('![](D:/ACAI_Wrapper/net/blue/6.png)')
			}
			datall3 <- rbind(datall3,dat_user1)
		}
		write.csv(datall3, "datall3.csv")
	}



	dat4 <- subset(acairm, select=c(fertilizer4, bags4, total_cost4, kgs4, currency,field_area))
	if(is.na(dat4$bags4) | dat4$bags4==0){
		fn <- "datall4.csv"
		if (file.exists(fn)) file.remove(fn)
	}else{
		datall4<-NULL
		for (i in 1:nrow(dat4)){
			dat_user4<- dat4[i,]
			if (dat_user4$bags4==0.5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/half.png)')
			}else if (dat_user4$bags4==1){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/1.png)')
			} else if (dat_user4$bags4==1.5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/1_5.png)')
			}else if (dat_user4$bags4==2){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/2.png)')
			}else if (dat_user4$bags4==2.5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/2_5.png)')
			}else if (dat_user4$bags4==3){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/3.png)')
			}else if (dat_user4$bags4==3.5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/3_5.png)')
			}else if (dat_user4$bags4==4){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/4.png)')
			}else if (dat_user4$bags4==4.5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/4_5.png)')
			}else if (dat_user4$bags4==5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/5.png)')
			}else if (dat_user4$bags4==5.5){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/5_5.png)')
			}else if (dat_user4$bags4==6){
				dat_user4$rep4 <- sprintf('![](D:/ACAI_Wrapper/net/red/6.png)')
			}
			datall4 <-rbind(datall4,dat_user4)
		}
		write.csv(datall4, "datall4.csv")
	}


	if(min(acairm$sum_total, acairm$revenue) == acairm$sum_total  ){
		ratioFertCost <- 1
		ratioTotalSale <-  round(acairm$totalSalePrice/acairm$sum_total, digits=0)
		ratioRevenue <-  round(acairm$revenue/acairm$sum_total, digits=0)
	}else{
		ratioFertCost <- round(acairm$sum_total/acairm$revenue, digits=0)
		ratioTotalSale <-  round(acairm$totalSalePrice/acairm$revenue, digits=0)
		ratioRevenue <- 1
	}


	acairm$revenue <- formatC(signif(acairm$revenue, digits=3), format="f", big.mark=",", digits=0)
	acairm$totalSalePrice <- formatC(signif(acairm$totalSalePrice, digits=3), format="f", big.mark=",", digits=0)
	acairm$sum_total <- formatC(signif(acairm$sum_total, digits=3), format="f", big.mark=",", digits=0)


	totalCostmoney <- data.frame(title=paste( acairm$sum_total, acairm$currency, sep=" "))
	totalSalemoney <- data.frame(title=paste( acairm$totalSalePrice, acairm$currency, sep=" "))
	totalRevenuemoney <- data.frame(title=paste( acairm$revenue, acairm$currency, sep=" "))



	if (ratioFertCost==1){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture1.png)')
	} else if (ratioFertCost==2){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture2.png)')
	}else if (ratioFertCost==3){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture3.png)')
	}else if (ratioFertCost==4){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture4.png)')
	}else if (ratioFertCost==5){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture5.png)')
	}else if (ratioFertCost==6){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture6.png)')
	}else if (ratioFertCost==7){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture7.png)')
	}else if (ratioFertCost==8){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture8.png)')
	}else if (ratioFertCost==9){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture9.png)')
	}else if (ratioFertCost==10){
		totalCostmoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture10.png)')
	}
	write.csv(totalCostmoney, "totalCostmoney.csv", row.names = FALSE)

	if (ratioTotalSale==1){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture1.png)')
	} else if (ratioTotalSale==2){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture2.png)')
	}else if (ratioTotalSale==3){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture3.png)')
	}else if (ratioTotalSale==4){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture4.png)')
	}else if (ratioTotalSale==5){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture5.png)')
	}else if (ratioTotalSale==6){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture6.png)')
	}else if (ratioTotalSale==7){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture7.png)')
	}else if (ratioTotalSale==8){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture8.png)')
	}else if (ratioTotalSale==9){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture9.png)')
	}else if (ratioTotalSale==10){
		totalSalemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture10.png)')
	}
	write.csv(totalSalemoney, "totalSalemoney.csv", row.names = FALSE)


	if (ratioRevenue==1){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture1.png)')
	} else if (ratioRevenue==2){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture2.png)')
	}else if (ratioRevenue==3){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture3.png)')
	}else if (ratioRevenue==4){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture4.png)')
	}else if (ratioRevenue==5){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture5.png)')
	}else if (ratioRevenue==6){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture6.png)')
	}else if (ratioRevenue==7){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture7.png)')
	}else if (ratioRevenue==8){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture8.png)')
	}else if (ratioRevenue==9){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture9.png)')
	}else if (ratioRevenue==10){
		totalRevenuemoney$moneypack <- sprintf('![](D:/ACAI_Wrapper/net/cash/Picture10.png)')
	}
	write.csv(totalRevenuemoney, "totalRevenuemoney.csv", row.names = FALSE)

}


#'  @param PD: controls not to ask recommendation for dates that are not considerd as planing dates in the zone
#'  @param rootUP: root rpice for fresh wt. ton/ha it is 270 in TZ and  141 for NG#'
#'  @param cassUW: Unit weight at which cassava produce is sold, in kg; common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag) and 1000 (per tonne); can be NA if user does not know.#'
#'  @param cassUP: Price of 1 cassava produce unit in local currency; can be NA if user does not know.
#'  @param maxInv: : Maximal investment in fertilizer, for the area of the field in local currency, NA if FR != TRUE, default = NA (if user does not wish to set an investment ceiling)
#'  @param rootUP: root price for fresh wt. in ton. It is 270$ in TZ and 141$ for NG
#'  @param country: "NG" or "TZ"
#'  @param lat
#'  @param lon
#'  @param areaUnits  : Units for area (acre, ha or m2)
#'  @param area       : Numerical value for area of the field
#'  @export
# PD = "2018-03-01"; HD = "2019-05-31"; lat = 10.024; lon = 4.025; country = "NG"; cassUW = 1000; cassUP = 50760; maxInv = 72000;
# urea=TRUE; ureaCostperBag=NULL; MOP=TRUE; MOPBagWt=NULL; MOPCostperBag=NULL; NPK201010=TRUE;NG_CY_Fertdata = NG_CY_FertRecom ;
# NPK201010BagWt=NULL; NPK201010CostperBag=NULL; NPK151515=TRUE; NPK151515BagWt=NULL; NPK151515CostperBag=NULL; area=2; areaUnits="acre";
# SoilData = SoilGridData_NG ; userName = "Pieter Pypers"; userPhoneCC = 254; userPhoneNr = 702974480;userEmail = "meklitc4@gmail.com"; cassPD = "roots"
# fieldDescription = "Cassava filed at the valley"


getFRrecommendations <- function(lat, lon, PD, HD, country, cassUW, cassUP, maxInv, fertilizers,
		NG_CY_Fertdata , SoilData, area, areaUnits, userName,
		userPhoneCC=NA,userPhoneNr=NA, cassPD, fieldDescription,userEmail ){
	if(lat < 5.5 | lat > 10.5 | lon<2.5 | lon > 9.5){
		return("Your location is outside of the area for which recommendations have been have been developed.")
	}else{

		## 1. define root price in ton/ha for fresh weight ## NOTE: shold be indicated in the app if it is dry weight or fresh weight price
		if(is.na(cassUP)| is.na(cassUW)){
			if(country == "TZ"){
				rootUP <- 607500# 270 * 2250
				country <- "Tanzania"
			}else{
				rootUP <- 50760# 141 * 360
				country <- "Nigeria"
			}
		}else if(!is.na(cassUP) & !is.na(cassUW)){
			if(cassUW == 1000){
				rootUP <- cassUP
			}else{
				cassUW <- cassUW/1000
				rootUP <- cassUP * cassUW
			}
		}


		## 2. get WLY, CY, fert recom and soil data
		WLY_FertRecom <- Onepx_WLY_CY(lat=lat, lon=lon, PD=PD, HD=HD , NG_CY_FertRecom)
		water_limited_yield <- WLY_FertRecom$water_limited_yield
		CurrentYield <- WLY_FertRecom$CurrentYield
		soilGPS <- SoilGridData_NG[SoilGridData_NG$latlong == WLY_FertRecom$location, ]


		## 3. fertilizers
		# fertilizer <- fertilizerFunc(urea, ureaBagWt, ureaCostperBag, MOP, MOPBagWt, MOPCostperBag, NPK201010,
		#                 NPK201010BagWt, NPK201010CostperBag, NPK151515, NPK151515BagWt, NPK151515CostperBag)
		#
		fertilizer <- fertilizerFunc2(fertData=fertilizers, country="Nigeria")


		## 4. optimize the fertilizer recommendation for maxInv in USD and provide expected target yield
		fert_optim <- run_Optim_NG2(rootUP=rootUP, QID=soilGPS, fertilizer=fertilizer, invest=maxInv, plDate=WLY_FertRecom$pl_Date,
				fert_onePixel=WLY_FertRecom, lat=lat, lon=lon)

		## 5. remove ferilizer application < 25 kg/ha
		fert_optim$urea <- suppressWarnings(ifelse(!is.na(fert_optim$urea) & fert_optim$urea <25, 0, fert_optim$urea))
		fert_optim$NPK151515 <- suppressWarnings(ifelse(!is.na(fert_optim$NPK151515) & fert_optim$NPK151515 < 25, 0, fert_optim$NPK151515))
		fert_optim$NPK201010 <- suppressWarnings(ifelse(!is.na(fert_optim$NPK201010) & fert_optim$NPK201010 <25, 0, fert_optim$NPK201010))
		fert_optim$MOP <- suppressWarnings(ifelse(!is.na(fert_optim$MOP) & fert_optim$MOP <25, 0, fert_optim$MOP))


		## 6. rerun to get target yield and respective cost and revenue
		ff <- c(fert_optim$urea, fert_optim$NPK151515, fert_optim$NPK201010, fert_optim$MOP)
		if(any(ff[!is.na(ff)] <25)){
			Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, wdd=WLY_FertRecom, rdd=fert_optim, fertilizer=fertilizer, Quefts_Input_Data=soilGPS)
		}else{
			fert_optim$rateUrea <- fert_optim$urea
			fert_optim$rateNPK151515 <- fert_optim$NPK151515
			fert_optim$rateNPK201010 <- fert_optim$NPK201010
			fert_optim$rateMOP <- fert_optim$MOP
		}


		Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, wdd=WLY_FertRecom, rdd=fert_optim, fertilizer=fertilizer, Quefts_Input_Data=soilGPS)

		## 7. if maxInvs with < 18% revenue, se fertilizer recom , net revenue and total cost to zero
		GPS_fertRecom <- NRabove18Cost(ds=Reset_fert_Cont)

		## 8. change recommendations to teh size of area provided

		if(areaUnits == "acre"){
			areaHa <- area/2.47
		}else if(areaUnits == "m2"){
			areaHa <- area/10000
		}else if(areaUnits == "ha"){
			areaHa <- area
		}


		GPS_fertRecom <- suppressWarnings(data.frame(lapply(GPS_fertRecom, function(x) as.numeric(as.character(x)))))

		GPS_fertRecom$areaUnits <- areaUnits
		GPS_fertRecom$area <- area
		GPS_fertRecom$rateUrea_user <- GPS_fertRecom$rateUrea * areaHa
		GPS_fertRecom$rateNPK151515_user <- GPS_fertRecom$rateNPK151515 * areaHa
		GPS_fertRecom$rateNPK201010_user <- GPS_fertRecom$rateNPK201010 * areaHa
		GPS_fertRecom$rateMOP_user <- GPS_fertRecom$rateMOP * areaHa
		GPS_fertRecom$currentY_user <- GPS_fertRecom$currentY  * areaHa
		GPS_fertRecom$targetY_user <- GPS_fertRecom$targetY * areaHa
		GPS_fertRecom$WLY_user <- GPS_fertRecom$WLY * areaHa
		GPS_fertRecom$netRev_user <- round(GPS_fertRecom$netRev * areaHa, digits=0)
		GPS_fertRecom$totalCost_user <- round(GPS_fertRecom$totalCost * areaHa)
		GPS_fertRecom$N_user <- GPS_fertRecom$N * areaHa
		GPS_fertRecom$P_user <- GPS_fertRecom$P * areaHa
		GPS_fertRecom$K_user <- GPS_fertRecom$K * areaHa


		fertcost <- suppressWarnings(GPS_fertRecom[, c("rateUrea_user", "rateNPK151515_user", "rateNPK201010_user", "rateMOP_user")])
		fertcost1 <- data.frame(fName = c("urea", "NPK151515", "NPK201010", "MOP"), fRate=round(as.numeric(gsub("NA", 0,fertcost)), digits=0))

		if(any(fertilizer$type == 'urea')){
			fd1 <- fertilizer[fertilizer$type == "urea", ]
			fertilizer1 <- "Urea"
			cost1 <- fd1$costPerBa
			unit1 <- paste(fd1$bagWeight, "kg bag", sep="")
			fertcost1$price[1] <- fd1$price
		}else{
			fertilizer1 <- "Urea"
			cost1 <- NA
			unit1 <- NA
			fertcost1$price[1] <- 0
		}

		if(any(fertilizer$type == 'NPK_15_15_15')){
			fd2 <- fertilizer[fertilizer$type == "NPK_15_15_15", ]
			fertilizer2 <- "NPK151515"
			cost2 <- fd2$costPerBag
			unit2 <- paste(fd2$bagWeight, "kg bag", sep="")
			fertcost1$price[2] <- fd2$price
		}else{
			fertilizer2 <- "NPK151515"
			cost2 <- NA
			unit2 <- NA
			fertcost1$price[2] <- 0
		}

		if(any(fertilizer$type == 'NPK_20_10_10')){
			fd3 <- fertilizer[fertilizer$type == "NPK_20_10_10", ]
			fertilizer3 <- "NPK201010"
			cost3 <- fd3$costPerBag
			unit3 <- paste(fd3$bagWeight, "kg bag", sep="")
			fertcost1$price[3] <- fd3$price
		}else{
			fertilizer3 <- "NPK201010"
			cost3 <- NA
			unit3 <- NA
			fertcost1$price[3] <- 0
		}

		if(any(fertilizer$type == 'MOP')){
			fd4 <- fertilizer[fertilizer$type == "MOP", ]
			fertilizer4 <- "MOP"
			cost4 <- fd4$costPerBag
			unit4 <- paste(fd4$bagWeight, "kg bag", sep="")
			fertcost1$price[4] <- fd4$price
		}else{
			fertilizer4 <- "MOP"
			cost4 <- NA
			unit4 <- NA
			fertcost1$price[4] <-0
		}



		fertcost <- round(fertcost1$fRate * fertcost1$price, digits=0)

		is.even <- function(x) x %% 2 == 0


		if(!is.na(GPS_fertRecom$rateUrea_user)) {
			bg1 <- round(GPS_fertRecom$rateUrea_user / 25, digits=0)
			if( is.even(bg1)==TRUE){
				bags1 <- bg1/2
			}else{
				bags1 <-((bg1-1)/2) + 0.5
			}
			kgs1 <- round(GPS_fertRecom$rateUrea_user, digits=0)
		}else{
			bags1 <- NA
			kgs1 <- NA
		}


		if(!is.na(GPS_fertRecom$rateNPK151515_user)){
			bg2 <- round(GPS_fertRecom$rateNPK151515_user / 25, digits=0)
			if(is.even(bg2)==TRUE){
				bags2 <-bg2/2
			}else{
				bags2 <-((bg2-1)/2) + 0.5
			}
			kgs2 <- round(GPS_fertRecom$rateNPK151515_user, digits=0)
		}else{
			bags2 < NA
			kgs2 <- NA
		}


		if(!is.na(GPS_fertRecom$rateNPK201010_user)){
			bg3 <- round(GPS_fertRecom$rateNPK201010_user / 25, digits=0)
			if(is.even(bg3)==TRUE){
				bags3 <- bg3/2
			}else{
				bags3 <-((bg3-1)/2) + 0.5
			}
			kgs3 <- round(GPS_fertRecom$rateNPK201010_user, digits=0)
		}else{
			kgs3 <- NA
			bags3 <- NA
		}



		if(!is.na(GPS_fertRecom$rateMOP_user)){
			bg4 <- round(GPS_fertRecom$rateMOP_user / 25, digits=0)
			if(is.even(bg4)==TRUE){
				bags4 <- bg4/2
			}else{
				bags4 <- ((bg4-1)/2) + 0.5
			}
			kgs4 <- round(GPS_fertRecom$rateMOP_user, digits=0)
		}else{
			kgs4 <- NA
			bags4 < -NA
		}

		prod_increase <- paste(GPS_fertRecom$targetY_user - GPS_fertRecom$currentY_user, " tonnes of cassava roots", sep="") ## dry wt
		prod_increase2 <- paste(GPS_fertRecom$targetY_user - GPS_fertRecom$currentY_user, " tonnes", sep="") ## dry wt
		bags_total <- ((GPS_fertRecom$targetY_user - GPS_fertRecom$currentY_user)*1000)/cassUW##
		phone <- as.character(if(!is.na(userPhoneCC)){paste(userPhoneCC, userPhoneNr, sep="")}else{NA})
		totalSalePrice <- round(GPS_fertRecom$netRev_user + GPS_fertRecom$totalCost_user, digits=0)
		field_area <- paste(area, areaUnits, sep=" ")
		cy <- formatC(signif(GPS_fertRecom$currentY_user, digits=3), format="f", big.mark=",", digits=0)

		current_yield <-paste(cy, " tonnes per ", GPS_fertRecom$area, GPS_fertRecom$areaUnits, sep="")
		currency <- if(country == "Nigeria"|country == "NG"){"NGN"}else{"TSH"}
		unitcassava <- paste(cassUW, "kg bag of ", cassPD, sep="" )
		sum_total <- round(GPS_fertRecom$totalCost_user, digits=0)
		revenue <- round(GPS_fertRecom$netRev_user, digits=0)


		T_csv <- data.frame(name=userName, phone=phone, field=fieldDescription, field_area=field_area, unit_field = areaUnits,
				plant_date = PD, hvst_date = HD,
				current_yield=current_yield, email= userEmail, latitude =lat, longitude=lon,currency,fertilizer1, cost1, unit1, kgs1,
				fertilizer2, cost2, unit2, kgs2, fertilizer3, cost3, unit3, kgs3, fertilizer4, cost4, unit4, kgs4, costcassava=cassUP,
				unitcassava=unitcassava, maxinvest=maxInv, bags1=bags1,rep1=NA, total_cost1=fertcost[1],bags2=bags2,rep2=NA,
				total_cost2=fertcost[2], bags3=bags3,rep3=NA, total_cost3=fertcost[3],bags4=bags4,rep4=NA, total_cost4= fertcost[4],
				sum_total, bags_total=bags_total, product=cassPD, unit="Kg", totalSalePrice= totalSalePrice , revenue  )

		fnc <- "MarkDownTextD.csv"
		if (file.exists(fnc)) file.remove(fnc)
		write.csv(T_csv, "MarkDownTextD.csv", row.names = FALSE)

		fertcost1 <- droplevels(fertcost1[fertcost1$fRate >0, ])

		fText <- c()
		for(k in 1:nrow(fertcost1)){
			fc <- fertcost1[k, ]
			if(fc$fName == "NPK201010"){
				fc$fName <- "NPK20:10:10"
			}else if(fc$fName == "NPK151515"){
				fc$fName <- "NPK15:15:15"
			}else if(fc$fName == "NPK171717"){
				fc$fName <- "NPK17:17:17"
			}
			text1 <- paste(fc$fRate, ' kg of ', fc$fName, sep="")
			fText <- c(fText, text1)
		}

		sum_total <- formatC(signif(GPS_fertRecom$totalCost_user, digits=3), format="f", big.mark=",", digits=0)
		revenue <- formatC(signif(GPS_fertRecom$netRev_user, digits=3), format="f", big.mark=",", digits=0)

		recText <- paste("Hello ", T_csv$name, "! If you plant cassava on ", PD, " and harvest on ", HD, ", we recommend you apply ", paste(fText, collapse = ", "),
				" on your field of ", T_csv$field_area, ". This investment will cost you ", sum_total, " ", T_csv$currency, ". ",
				"We expect you will increase your cassava root production by ", prod_increase2, " and your net returns by ",
				revenue, " ", T_csv$currency, ". Thank you for using our services!", sep="" )

		return(list(GPS_fertRecom, T_csv, recText))
	}

}



#' @param for a location get the closests pixel with WLY and CY, get the closest planting date and harvesting dates as well
#' @param PD <- "2018-03-16"
#' @param HD <- "2019-05-25"
#' @param lat <- 6.225
#' @param lon <- 4.675
#' @param NG_CY_FertRecom is QUEEFTS output with lat, lon, CY, WLY, nutrient rates NPK, planting dates and harvest dates
#' @export
Onepx_WLY_CY <- function(lat, lon, PD, HD, NG_CY_Fertdata){
	pDate <-  datesIn365(PD, pl=TRUE)
	hDate <-  datesIn365(HD, hv=TRUE)
	## if planting and harvestins is in different years the days in the planting year should be added to the days in harvesting year
	if(pDate$year_pl < hDate$year_hv){
		hDate$day_hv <- hDate$day_hv + (365 - pDate$day_pl)
	}
	## subset WLY, Cu and fert recom for rteh planting and harvest days
	possiblePlDate <- data.frame(p_pl= as.numeric(unique(NG_CY_Fertdata$pl_Date)), a_pl = pDate$day_pl)
	possiblePlDate$diffl <- abs(possiblePlDate$p_pl - possiblePlDate$a_pl)
	plantingDate <- possiblePlDate[possiblePlDate$diffl == min(possiblePlDate$diffl),]$p_pl

	possibleHvDate <- data.frame(p_hv= as.numeric(unique(NG_CY_Fertdata$daysOnField)), a_hv = hDate$day_hv)
	possibleHvDate$diffl <- abs(possibleHvDate$p_hv - possibleHvDate$a_hv)
	harvestDate <- possibleHvDate[possibleHvDate$diffl == min(possibleHvDate$diffl),]$p_hv
	fertRecom_dates <- droplevels(NG_CY_Fertdata[NG_CY_Fertdata$pl_Date == plantingDate & NG_CY_Fertdata$daysOnField == harvestDate, ])

	## subset for lat and lon
	point_px <- data.frame(lat=lat, lon=lon)
	fertRecom_dates$latDiff <- abs(fertRecom_dates$lat - lat)
	minlat_coor <- droplevels(fertRecom_dates[fertRecom_dates$latDiff == min(fertRecom_dates$latDiff), ])

	minlat_coor$lonDiff <- abs(minlat_coor$long - lon)
	minlatlon_coor <- droplevels(minlat_coor[minlat_coor$lonDiff == min(minlat_coor$lonDiff), ])
	rownames(minlatlon_coor) <- NULL
	minlatlon_coor <- subset(minlatlon_coor, select = -c(latDiff, lonDiff))
	return(minlatlon_coor)
}



#' function to creat a data frae with fertilizers
#' @param urea logical tru false if it is avaiable for farmers
#' @param ureaBagWt if user define weight of urea in a bag
#' @param ureaCostperBag the cost of urea in defined bag
#' like wise
#' @example  urea=FALSE; ureaBagWt=NULL; ureaCostperBag=NULL; MOP=TRUE; MOPBagWt=NULL; MOPCostperBag=NULL;NPK201010=TRUE; NPK201010BagWt=NULL; NPK201010CostperBag=NULL
#' NPK151515=TRUE; NPK151515BagWt=NULL; NPK151515CostperBag=NULL
fertilizerFunc <- function(urea=TRUE, ureaBagWt=NULL, ureaCostperBag=NULL,
		MOP=TRUE, MOPBagWt=NULL, MOPCostperBag=NULL,
		NPK201010=TRUE, NPK201010BagWt=NULL, NPK201010CostperBag=NULL,
		NPK151515=TRUE, NPK151515BagWt=NULL, NPK151515CostperBag=NULL, country){

	fertilizers2 <- expand.grid(type = c("urea", "CAN", "NPK17:17:17", "NPK15:15:15", "NPK20:10:10", "DAP", "TSP", "SSP", "Minjingu_Nafaka+", "MOP"),
			                                                       available = FALSE,
			                                                          price = NA,
			                                                          weight = 50)


	if(urea==TRUE){
		ureaFert <- data.frame(type = 'urea', N_cont = 0.46, P_cont = 0, K_cont=0, Zones = "Nigeria")
		if(!is.null(ureaCostperBag) & !is.null(ureaBagWt)){
			ureaFert$costPerBag <- ureaCostperBag
			ureaFert$bagWeight <- ureaBagWt
			ureaFert$price50 <- ureaCostperBag * (50/ureaBagWt)## should be per 50 kg of bag?
			ureaFert$price <- ureaFert$price50/50
		}else{
			ureaFert$costPerBag <- 7182#19.95*360
			ureaFert$bagWeight <- 50
			ureaFert$price50 <- 7182
			ureaFert$price <- 143.64
		}
	}else{
		ureaFert <- NULL
	}

	if(MOP==TRUE){
		MOPFert <- data.frame(type = 'MOP', N_cont = 0.00, P_cont = 0.00, K_cont=0.50, Zones = "Nigeria")
		if(!is.null(MOPCostperBag) & !is.null(MOPBagWt)){
			MOPFert$costPerBag <- MOPCostperBag
			MOPFert$bagWeight <- MOPBagWt
			MOPFert$price50 <- MOPCostperBag * (50/MOPBagWt)
			MOPFert$price <- MOPFert$price50/50
		}else{
			MOPFert$costPerBag <- 11484#31.9*360
			MOPFert$bagWeight <- 50
			MOPFert$price50 <- 11484
			MOPFert$price <- 229.68
		}
	}else{
		MOPFert <- NULL
	}

	if(NPK201010==TRUE){
		NPK201010Fert <- data.frame(type = 'NPK_20_10_10', N_cont = 0.20, P_cont = 0.044, K_cont=0.083, Zones = "Nigeria")
		if(!is.null(NPK201010CostperBag) & !is.null(NPK201010BagWt)){
			NPK201010Fert$costPerBag <- NPK201010CostperBag
			NPK201010Fert$bagWeight <- NPK201010BagWt
			NPK201010Fert$price50 <- NPK201010CostperBag * (50/NPK201010BagWt)
			NPK201010Fert$price <- NPK201010Fert$price50/50
		}else{
			NPK201010Fert$costPerBag <- 7200#20*360
			NPK201010Fert$bagWeight <- 50
			NPK201010Fert$price50 <- 7200
			NPK201010Fert$price <- 144
		}
	}else{
		NPK201010Fert <- NULL
	}

	if(NPK151515==TRUE){
		NPK151515Fert <- data.frame(type = 'NPK_15_15_15', N_cont = 0.15, P_cont = 0.07, K_cont=0.125, Zones = "Nigeria")
		if(!is.null(NPK151515CostperBag) & !is.null(NPK151515BagWt)){
			NPK151515Fert$costPerBag <- NPK151515CostperBag
			NPK151515Fert$bagWeight <- NPK151515BagWt
			NPK151515Fert$price50 <- NPK151515CostperBag * (50/NPK151515BagWt)
			NPK151515Fert$price <- NPK151515Fert$price50/50
		}else{
			NPK151515Fert$costPerBag <- 6538#18.16*360
			NPK151515Fert$bagWeight <- 50
			NPK151515Fert$price50 <- 6538
			NPK151515Fert$price <- 130.76
		}
	}else{
		NPK151515Fert <- NULL
	}

	fertilizers <- rbind(ureaFert, NPK151515Fert, NPK201010Fert, MOPFert )


	return(fertilizers)
}


#' function to creat a data frae with fertilizers
#' @param urea logical tru false if it is avaiable for farmers
#' @param ureaBagWt if user define weight of urea in a bag
#' @param ureaCostperBag the cost of urea in defined bag
#' like wise
#' @example  fertilizerFunc2(fertData, country="Nigeria")

fertilizerFunc2 <- function(fertData, country){
	fertilizers <- fertData
	fertilizers$Zones <- country

	##TODO prices are for NG

	ureaFert <- data.frame(type = 'urea', N_cont = 0.46, P_cont = 0, K_cont=0, costPerBag = 7182, bagWeight=50,price50=7182, price=143.64 )
	MOPFert <- data.frame(type = 'MOP', N_cont = 0.00, P_cont = 0.00, K_cont=0.50, costPerBag = 11484, bagWeight=50,price50=11484, price=229.68)
	NPK201010Fert <- data.frame(type = 'NPK20:10:10', N_cont = 0.20, P_cont = 0.044, K_cont=0.083, costPerBag = 7200, bagWeight=50,price50=7200, price=144)
	NPK151515Fert <- data.frame(type = 'NPK15:15:15', N_cont = 0.15, P_cont = 0.07, K_cont=0.125, costPerBag = 6538, bagWeight=50,price50=6538, price=130.76)
	TSPFert <- data.frame(type = 'TSP', N_cont = 0.0, P_cont = 0.2, K_cont=0.0, costPerBag = 25408.8, bagWeight=50,price50=25408.8, price=508.176)
	DAPFert <- data.frame(type = 'DAP', N_cont = 0.18, P_cont = 0.2, K_cont=0.0, costPerBag = 0, bagWeight=50,price50=0, price=0)##TODO get price
	NPK171717Fert <- data.frame(type = 'NPK17:17:17', N_cont = 0.17, P_cont = 0.083, K_cont=0.15, costPerBag = 0, bagWeight=50,price50=0, price=0)# TODO get price
	MPRNafakaFert <- data.frame(type = 'Minjingu_Nafaka+', N_cont = 0.09, P_cont = 0.07, K_cont=0.06, costPerBag = 0, bagWeight=50,price50=0, price=0)
	#MPRMazaoFert <- data.frame(type = 'MPRMazao', N_cont = 0.10, P_cont = 0.09, K_cont=0.0)
	CANFert <- data.frame(type = 'CAN', N_cont = 0.01, P_cont = 0.01, K_cont=0.01,costPerBag = 0, bagWeight=50,price50=0, price=0)## not correct value TODO check
	SSPFert <- data.frame(type = 'SSP', N_cont = 0.01, P_cont = 0.01, K_cont=0.01, costPerBag = 22363.6, bagWeight=50,price50=22363.2, price=447.26)## not correct value TODO check

	fd_cont <- rbind(ureaFert,CANFert,NPK171717Fert,NPK151515Fert, NPK201010Fert, DAPFert,TSPFert,SSPFert,MPRNafakaFert,MOPFert )


	Fd_get <- NULL
	for(i in 1:nrow(fertilizers)){
		fd <- fertilizers[i, ]
		fdc <- fd_cont[i, ]

		if(fd$type == "NPK20:10:10"){
			fd$type <- "NPK_20_10_10"
			fdc$type <- "NPK_20_10_10"
		}else if(fd$type == "NPK15:15:15"){
			fd$type <- "NPK_15_15_15"
			fdc$type <- "NPK_15_15_15"
		}else if(fd$type == "NPK17:17:17"){
			fd$type <- "NPK_17_17_17"
			fdc$type <- "NPK_17_17_17"
		}

		if(fd$available==TRUE){
#			fd$available <- fd$available
			fd$N_cont <- fdc$N_cont
			fd$P_cont <- fdc$P_cont
			fd$K_cont <- fdc$K_cont
			if(!is.na(fd$price) & !is.na(fd$weight)){
				fd$costPerBag <- fd$price
				fd$bagWeight <- fd$weight
				fd$price50 <- (fd$price * 50)/fd$weight## should be per 50 kg of bag?
				fd$price <- fd$price50/50
			}else{
				fd$costPerBag <- fdc$costPerBag
				fd$bagWeight <- fdc$bagWeight
				fd$price50 <- fdc$price50
				fd$price <- fdc$price
			}
		}else{
			fd <- NULL
		}
		Fd_get <- rbind(Fd_get, fd)
	}

	return(Fd_get)
}



#' function to convert date 1:30/31 to 1:365/366
#' @param ddf is date, in format "yyyy-mm-dd"
#' @example datesIn365(ddf="2018-06-03")
datesIn365 <- function(ddf, hv=FALSE, pl=FALSE){
	year = as.numeric(strsplit(ddf, "-")[[1]][1])
	month = as.numeric(strsplit(ddf, "-")[[1]][2])
	date = as.numeric(strsplit(ddf, "-")[[1]][3])

	if(year %in% c(2016, 2020, 2024, 2028, 2032)){
		leapyear <- TRUE
	}else{
		leapyear <- FALSE
	}

	if(leapyear==TRUE){
		if(month == 1){
			dd <- date
		}else if(month == 2){
			dd <- date + 31
		}else if(month == 3){
			dd <- date + 60
		}else if(month == 4){
			dd <- date + 91
		}else if(month == 5){
			dd <- date + 121
		}else if(month == 6){
			dd <- date + 152
		}else if(month == 7){
			dd <- date + 182
		}else if(month == 8){
			dd <- date + 213
		}else if(month == 9){
			dd <- date + 244
		}else if(month == 10){
			dd <- date + 274
		}else if(month == 11){
			dd <- date + 305
		}else if(month == 12){
			dd <- date + 335
		}
	}else{
		if(month == 1){
			dd <- date
		}else if(month == 2){
			dd <- date + 31
		}else if(month == 3){
			dd <- date + 59
		}else if(month == 4){
			dd <- date + 90
		}else if(month == 5){
			dd <- date + 120
		}else if(month == 6){
			dd <- date + 151
		}else if(month == 7){
			dd <- date + 181
		}else if(month == 8){
			dd <- date + 212
		}else if(month == 9){
			dd <- date + 243
		}else if(month == 10){
			dd <- date + 273
		}else if(month == 11){
			dd <- date + 304
		}else if(month == 12){
			dd <- date + 334
		}
	}
	if(pl==TRUE){
		return(data.frame(userDate_pl = ddf, year_pl=year, month_pl=month, date_pl=date, day_pl=dd))
	}
	if(hv==TRUE){
		return(data.frame(userDate_hv = ddf, year_hv=year, month_hv=month, date_hv=date, day_hv=dd))
	}

}



actual_uptake_tool <- function(ds_supply){
	with(ds_supply,
			{
				UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
				UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
				UNW <- water_dependent_nutrient_uptake(S1 = SN, WLY = WLY, d1 = dN, a1 = aN, r1 = rN)
				UN <- min(UNP, UNK, UNW)


				UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
				UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
				UPW <- water_dependent_nutrient_uptake(S1 = SP, WLY = WLY, d1 = dP, a1 = aP, r1 = rP)
				UP <- min(UPN, UPK, UPW)


				UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
				UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
				UKW <- water_dependent_nutrient_uptake(S1 = SK, WLY = WLY, d1 = dK, a1 = aK, r1 = rK)
				UK <- min(UKN, UKP, UKW)


				return(data.frame(UN=UN, UP=UP, UK=UK))
			})
}


water_dependent_nutrient_uptake <- function(S1=NA, WLY=NA, d1=NA, a1=NA, r1=NA) {
	if (S1 < r1 + WLY / d1) {
		uptakeX_givenWater = S1
	} else if (S1 > r1 + 2*WLY/a1 - WLY/d1) {
		uptakeX_givenWater = WLY / a1
	} else {
		uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY/d1)^2 / (WLY / a1 - WLY / d1)
	}

	return(uptakeX_givenWater)
}


#' Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
nutrient_uptake <- function(S1=NA, S2=NA, d1=NA, a1=NA, d2=NA, a2=NA, r1=NA, r2=NA) {
	# N, P and K uptakes based on QUEFTS
	if (S1 < r1 + ((S2 - r2) * a2 / d1)) {
		uptakeX_givenY = S1
	} else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))) {
		uptakeX_givenY = r1 + (S2 - r2) * (d2 / a1)
	} else {
		uptakeX_givenY = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
	}
	# Nutrient uptake given availability of other nutrient
	return(uptakeX_givenY)
}

max_min_yields_tools <- function(dss){

	YNA <- max((dss$UN - dss$rN), 0) * dss$aN
	YND <- max((dss$UN - dss$rN), 0) * dss$dN
	YPA <- max((dss$UP - dss$rP), 0) * dss$aP
	YPD <- max((dss$UP - dss$rP), 0) * dss$dP
	YKA <- max((dss$UK - dss$rK), 0) * dss$aK
	YKD <- max((dss$UK - dss$rK), 0) * dss$dK


	return(data.frame(YNA=YNA, YND=YND, YPA=YPA, YPD=YPD, YKA=YKA, YKD=YKD))

}


final_yield_tools <- function(Uptake_Yield){
	with(Uptake_Yield,
			{
				YNP <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YPA, Y2D = YPD, Y3D = YKD, r1 = rN)
				YNK <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YKA, Y2D = YKD, Y3D = YPD, r1 = rN)
				YPN <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YNA, Y2D = YND, Y3D = YKD, r1 = rP)
				YPK <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YKA, Y2D = YKD, Y3D = YND, r1 = rP)
				YKN <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YNA, Y2D = YND, Y3D = YPD, r1 = rK)
				YKP <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YPA, Y2D = YPD, Y3D = YND, r1 = rK)

				# Make sure the nutrient limited yields do not exceed the maximum possible yield = WLY
				YNPc <- min(c(YNP, YND, YPD, YKD, WLY))
				YNKc <- min(c(YNK, YND, YPD, YKD, WLY))
				YPNc <- min(c(YPN, YND, YPD, YKD, WLY))
				YPKc <- min(c(YPK, YND, YPD, YKD, WLY))
				YKNc <- min(c(YKN, YND, YPD, YKD, WLY))
				YKPc <- min(c(YKP, YND, YPD, YKD, WLY))

				#Final estimate
				YEc <- mean(c(YNPc, YNKc, YPNc, YPKc, YKNc, YKPc))

				return(YEc)
			})
}

#' Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.
yield_nutrients_combined <- function(U1=NA, d1=NA, a1=NA, Y2A=NA, Y2D=NA, Y3D=NA, r1=NA){
	# Determine which nutrient limited yield is lowest.
	YxD = min(Y2D, Y3D)
	# If the uptake of one of the nutrients, and therefore the yield associated with that
	# nutrient, is zero the overall yield is also zero.
	if (U1 == 0 || YxD == 0) {
		Y12 = 0
	}else{
		Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
				(YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
	}
	# Return the calculated yield based on the uptake of nutrients 1 and 2
	return(Y12)
}




quefts_tools <- function(supply_wly){
	# Actual uptake of nutrients.
	tmp <- actual_uptake_tool(supply_wly)
	supply_wly$UN <- tmp[[1]]
	supply_wly$UP <- tmp[[2]]
	supply_wly$UK <- tmp[[3]]

	# Maximum and minimum yields, depending on maximum accumulation and dilution.
	yields <- max_min_yields_tools(supply_wly)
	supply_wly$YNA <- yields$YNA
	supply_wly$YND <- yields$YND
	supply_wly$YPA <- yields$YPA
	supply_wly$YPD <- yields$YPD
	supply_wly$YKA <- yields$YKA
	supply_wly$YKD <- yields$YKD

	# Final yield based on the combinations of nutrient uptake and minimum + maximum yields.
	supply_wly$FinalYield <- final_yield_tools(supply_wly)

	return(supply_wly)
}



#' using the output of function "NPK_TargetYield_forinput" and a dat frame per lon and lat for intended NPK input
#' this function calculates the yield that can be obtained for intended NPK rate.
#' @param NutrUse_soilNPK
#' @param NPKdata: needs to be provided
#' @return
#'
#' @author Meklit
#' @export
NPK_TargetYield_forOutput <- function(NutrUse_soilNPK, N_rate, P_rate, K_rate){
	NutrUse_soilNPK$N_rate <- N_rate
	NutrUse_soilNPK$P_rate <- P_rate
	NutrUse_soilNPK$K_rate <- K_rate

	## Supply of nutrients to the crop
	NutrUse_soilNPK$SN <- NutrUse_soilNPK$N_rate + NutrUse_soilNPK$soilN
	NutrUse_soilNPK$SP <- NutrUse_soilNPK$P_rate + NutrUse_soilNPK$soilP
	NutrUse_soilNPK$SK <- NutrUse_soilNPK$K_rate + NutrUse_soilNPK$soilK

	## Actual Uptake of nutrients: crop param + nutrient supply
	tmp <- ddply(NutrUse_soilNPK,.(lat, long), actual_uptake_tool)
	NutrUse_soilNPK <- merge(NutrUse_soilNPK, tmp, by=c("lat", "long"))

	## max and min yield: actual uptake and crop param. min of N uptake constrianed by availability of P, K and water
	maxminY <- ddply(NutrUse_soilNPK,.(lat, long), max_min_yields_tools)
	NutrUse_soilNPK <- merge(NutrUse_soilNPK, maxminY, by=c("lat", "long"))

	## final yield: min yield for combined uptake of 2 nutrients assuming the 3rd is not limiting, should be < WLY, and take meanof the six combinations
	Target_Yield <- ddply(NutrUse_soilNPK,.(lat, long), quefts_tools)
	TY <- data.frame(lat=Target_Yield$lat, lon=Target_Yield$long, TargetYield=Target_Yield$FinalYield)

	return(TY)
}




#' computes target yield in tonnes/ha from a given NPK rate
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.),
#' @param rec recomended NPK rate
#' @returnType
#' @return target yield in ton/ha dry matter
#'
#' @author Meklit
#' @export
QUEFTS1_Pedotransfer <- function(QID, rec){
	QID$WLY <- QID$water_limited_yield

	crop_param <- cbind(NUE(HI=0.52), data.frame(rN=0, rP=0, rK=0, max_yield=QID$WLY, tolerance=0.01))	## nutrient use efficiency of the crop


	Queft_Input_Data_Var1 <- cbind(QID, crop_param)
	indata <- Queft_Input_Data_Var1[,c("lat","long" ,"WLY","aN", "dN", "aP", "dP","aK","dK", "rN", "rP", "rK", "soilN", "soilP", "soilK","max_yield", "tolerance")]

	N_rate <- rec[1]
	P_rate <- rec[2]
	K_rate <- rec[3]

	TargetYield_from_NPK <- NPK_TargetYield_forOutput(NutrUse_soilNPK=indata, N_rate, P_rate, K_rate)

	return(TargetYield_from_NPK$TargetYield)
}


NUE <- function(HI, CmaxNroots=6.6, CminNroots=2.5, CmaxNtops=17.9, CminNtops=7.9, CmaxProots=1.5, CminProots=0.8, CmaxPtops=2.8, CminPtops=0.9,
		CmaxKroots=11, CminKroots=2.8, CmaxKtops=18.8, CminKtops=3.4 ){
	aN = round(1000 * HI/(HI * CmaxNroots + (1 - HI) * CmaxNtops), digits=0)
	dN = round(1000 * HI/(HI * CminNroots + (1 - HI) * CminNtops), digits=0)

	aP = round(1000 * HI/(HI * CmaxProots + (1 - HI) * CmaxPtops), digits=0)
	dP = round(1000 * HI/(HI * CminProots + (1 - HI) * CminPtops), digits=0)

	aK = round(1000 * HI/(HI * CmaxKroots + (1 - HI) * CmaxKtops), digits=0)
	dK = round(1000 * HI/(HI * CminKroots + (1 - HI) * CminKtops), digits=0)

	return(data.frame(aN=aN, dN=dN,aP=aP,dP=dP,aK=aK,dK=dK))

}


#' after setting fertilizer recommendation <25 kg/ha Urea, MOP or Nafaka, target yield with the remaining recommended fertilizer is  re-estimated  and
#'  total cost, gross and net revenue are re calcuated.
#' @param rootUP cassava root price
#' @param zone
#' @param wdd
#' @param rdd
#' @param fertilizer
#' @author Meklit
#' @export

Rerun_25kgKa_try <- function(rootUP, wdd, rdd, fertilizer, Quefts_Input_Data){

	fertilizers <- fertilizer
	recalc_data <- rdd
	QID <- Quefts_Input_Data
	fert_onePixel <- wdd
	QID$water_limited_yield <- fert_onePixel$water_limited_yield

	WLY <- fert_onePixel$water_limited_yield/1000
	CY <- (fert_onePixel$CurrentYield)/1000							#fresh wt yield in ton
	rec_FertAmount <- c(recalc_data$urea, recalc_data$NPK151515, recalc_data$NPK201010, recalc_data$MOP)

	rec_FertAmount <- rec_FertAmount[!is.na(rec_FertAmount)]

	TC <- sum(rec_FertAmount %*% fertilizers$price)

	N  <- as.vector(rec_FertAmount %*% fertilizer$N_cont)
	P  <- as.vector(rec_FertAmount %*% fertilizer$P_cont)
	K  <- as.vector(rec_FertAmount %*% fertilizer$K_cont)
	rec <- c(N, P, K)

	fertilizer$FR <- rec_FertAmount

	if(any(fertilizer$type == "urea")){
		rateUrea  <- fertilizer[fertilizer$type == "urea", "FR"]
	}else{
		rateUrea <- "NA"
	}

	if(any(fertilizer$type == "NPK_15_15_15")){
		rateNPK151515  <- fertilizer[fertilizer$type == "NPK_15_15_15", "FR"]
	}else{
		rateNPK151515 <- "NA"
	}

	if(any(fertilizer$type == "NPK_20_10_10")){
		rateNPK201010  <- fertilizer[fertilizer$type == "NPK_20_10_10", "FR"]
	}else{
		rateNPK201010 <- "NA"
	}

	if(any(fertilizer$type == "MOP")){
		rateMOP  <- fertilizer[fertilizer$type == "MOP", "FR"]
	}else{
		rateMOP <- "NA"
	}


	TY  <- (QUEFTS1_Pedotransfer(QID,rec))/1000						#fresh wt yield in ton
	if(TY <= CY){
		GR <- 0
		NR <- 0
	}else{
		GR  <- (TY - CY) * rootUP                       						#gross revenue
		NR  <- GR - TC
	}

	return(data.frame(lat=recalc_data$lat, lon=recalc_data$lon, rateUrea,  rateNPK151515, rateNPK201010,rateMOP, currentY= CY,
					targetY=TY, WLY=WLY, netRev=NR,totalCost = TC, N=N, P=P, K=K, plDate=recalc_data$plDate))
}




### see if profit is > 0.18 * total cost
NRabove18Cost <- function(ds){
	ds$return <- ds$totalCost * 0.18
	ds$SC <- ifelse(ds$netRev > ds$return, "Keep", "Change")

	fertRecom_profitable <- droplevels(ds[ds$SC=="Keep", ])
	fertRecom_NOTprofitable <- droplevels(ds[ds$SC=="Change", ])

	if(nrow(fertRecom_NOTprofitable)>0){
		fertRecom_NOTprofitable$rateUrea <- 0
		fertRecom_NOTprofitable$rateNPK151515  <- 0
		fertRecom_NOTprofitable$rateNPK201010  <- 0
		fertRecom_NOTprofitable$rateMOP <- 0
		fertRecom_NOTprofitable$ netRev <- 0
		fertRecom_NOTprofitable$totalCost <- 0
		fertRecom_NOTprofitable$N <- 0
		fertRecom_NOTprofitable$P <- 0
		fertRecom_NOTprofitable$K <- 0
		fertRecom_NOTprofitable$targetY <- fertRecom_NOTprofitable$currentY
	}

	fertRecom <- (rbind(fertRecom_NOTprofitable, fertRecom_profitable))

	fertRecom$currentY <- round(fertRecom$currentY, digits=2)
	fertRecom$targetY <- round(fertRecom$targetY, digits=2)
	fertRecom$WLY <- round(fertRecom$WLY, digits=2)
	fertRecom$totalCost <- round(fertRecom$totalCost, digits=0)
	fertRecom$netRev <- round(fertRecom$netRev, digits=0)
	fertRecom$N <- round(fertRecom$N, digits=0)
	fertRecom$P <- round(fertRecom$P, digits=0)
	fertRecom$K <- round(fertRecom$K, digits=0)
	fertRecom <- subset(fertRecom, select=-c(return, SC))

	return(fertRecom)
}



#'  Optimize the UREA, TSP and MOP needed to maximize the NR. x1, x2, x3 = Urea, MOP and Nafaka kg/ha.
optim_NR <- function(fertRate, rootUP, QID, CY, fertilizer, invest){
	f_price <-fertilizer$price
	TC <- sum(fertRate * f_price)

	## Kg of Urea, Kg of NPK151515, Kg of NPK201010, Kg of MOP

	N <- as.vector(fertRate %*% fertilizer$N_cont)
	P <- as.vector(fertRate %*% fertilizer$P_cont)
	K <- as.vector(fertRate %*% fertilizer$K_cont)

	rec <- c(N,P,K)

	TotalYield <- QUEFTS1_Pedotransfer(QID, rec)*3/1000
	AdditionalYield <- TotalYield - CY
	PriceYield <- AdditionalYield * rootUP
	NetRev <- PriceYield - TC

	if (!is.na(invest) & TC > invest) {NetRev <- NetRev - (invest-TC)^2} #penalize NR if costs exceed investment cap

	return(NetRev)
}





run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, plDate, fert_onePixel, lat, lon){

	QID$water_limited_yield <- fert_onePixel$water_limited_yield
	initial <- rep(0, nrow(fertilizer))
	lowerST <- rep(0, nrow(fertilizer))

	CY <- (fert_onePixel$CurrentYield)/1000 ## is fresh wt in t/ha
	FR <- optim(par=initial, fn=optim_NR, lower=lowerST, method = "L-BFGS-B", control=list(fnscale=-1), rootUP=rootUP,
			QID=QID, CY=CY, fertilizer=fertilizer, invest=invest)$par

	TC <- round(sum(FR * fertilizer$price), digits=0)

	fertilizer$FR <- FR

	if(any(fertilizer$type == "urea")){
		urea  <- fertilizer[fertilizer$type == "urea", "FR"]
	}else{
		urea <- "NA"
	}

	if(any(fertilizer$type == "NPK_15_15_15")){
		NPK151515  <- fertilizer[fertilizer$type == "NPK_15_15_15", "FR"]
	}else{
		NPK151515 <- "NA"
	}

	if(any(fertilizer$type == "NPK_20_10_10")){
		NPK201010  <- fertilizer[fertilizer$type == "NPK_20_10_10", "FR"]
	}else{
		NPK201010 <- "NA"
	}

	if(any(fertilizer$type == "MOP")){
		MOP  <- fertilizer[fertilizer$type == "MOP", "FR"]
	}else{
		MOP <- "NA"
	}

	N <- as.vector(FR %*% fertilizer$N_cont)
	P <- as.vector(FR %*% fertilizer$P_cont)
	K <- as.vector(FR %*% fertilizer$K_cont)

	rec <- c(N, P, K)

	TY <- round((QUEFTS1_Pedotransfer(QID, rec)/1000), digits=3)	# Yield possible at recommended NPK in t/ha fresh wt.
	GR <- (TY - CY) * rootUP                                        # Gross revenue given root up for fresh wt is 270
	NR <- round(GR - TC, digits=0)    											# Net Revenue
	return(data.frame(lat=lat, lon=lon, plDate, urea, NPK151515, NPK201010, MOP, NR=NR, N=N, P=P, K=K,TC, TY, CurrYield = CY,
					WLY=QID$water_limited_yield/1000))
}




#run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, plDate, fert_onePixel, lat, lon){
#
#	QID$water_limited_yield <- fert_onePixel$water_limited_yield
#
#	initial <- c(0,0,0,0)
#	CY <- (fert_onePixel$CurrentYield)/1000 ## is fresh wt in t/ha
#	FR <- optim(par=initial, optim_NR, lower=c(0,0,0,0), method = "L-BFGS-B", control=list(fnscale=-1), rootUP=rootUP,
#			QID=QID, CY=CY, fertilizer=fertilizer, invest=invest)$par
#
#	TC <- round(sum(FR * fertilizer$price), digits=0)							# total cost
#
#	urea <- FR[1]
#	NPK151515 <- FR[2]
#	NPK201010 <- FR[3]
#	MOP <- FR[4]
#
#	N <- as.vector(FR %*% fertilizer$N_cont)
#	P <- as.vector(FR %*% fertilizer$P_cont)
#	K <- as.vector(FR %*% fertilizer$K_cont)
#
#	rec <- c(N, P, K)
#
#	TY <- round((QUEFTS1_Pedotransfer(QID, rec)/1000), digits=3)	# Yield possible at recommended NPK in t/ha fresh wt.
#	GR <- (TY - CY) * rootUP                                # Gross revenue given root up for fresh wt is 270
#	NR <- round(GR - TC, digits=0)    											# Net Revenue
#	return(data.frame(lat=lat, lon=lon, plDate, urea, NPK151515, NPK201010, MOP, NR=NR, N=N, P=P, K=K,TC, TY, CurrYield = CY,
#					WLY=QID$water_limited_yield/1000))
#}
