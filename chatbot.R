######################################################################
#Initialisierung
######################################################################

#install.packages("telegram.bot")
library(telegram.bot)

bot <- Bot(token = bot_token("RTelegramBot"))
updater <- Updater(token = bot_token("RTelegramBot"))


######################################################################
#Start-Methode
######################################################################

start <- function(bot, update)
{
  text <- sprintf("Hello %s! How are you today, my friend?",
                 update$message$from$first_name)
  bot$sendMessage(chat_id = update$message$chat_id,text)

}
start_handler <- CommandHandler("start", start)
updater <- updater + start_handler

######################################################################
#Lets Talk (-> Question 0)
######################################################################

letstalk <- function(bot, chat_id)
{
  text <- paste("A little chat is always good. ", 
                "How sad do you feel today from a scale from 0 to 5?", sep="")
  
  keyboard <- list(
    list(
      InlineKeyboardButton(
        text=0,
        callback_data="question_0"
      ),
      InlineKeyboardButton(
        text=1,
        callback_data="question_1"
      ),
      InlineKeyboardButton(
        text=2,
        callback_data="question_2"
      ),
      InlineKeyboardButton(
        text=3,
        callback_data="question_3"
      ),
      InlineKeyboardButton(
        text=4,
        callback_data="question_4"
      ),
      InlineKeyboardButton(
        text=5,
        callback_data="question_5"
      )
    )
  )
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keyboard
  )
  
  bot$sendMessage(chat_id = chat_id,text, reply_markup=IKM)
}

letstalk_callback <- function(bot, update){
  query <- update$callback_query
  letstalk(bot, query$message$chat$id)
}

letstalk_command <- function(bot, update)
{
  letstalk(bot, update$message$chat_id)
}

letstalk_handler <- CommandHandler("letstalk", letstalk_command)
letstalk_callback_handler <- CallbackQueryHandler(letstalk_callback, pattern = "letstalk")
updater <- updater + letstalk_handler + letstalk_callback_handler

######################################################################
# Question (-> Result)
######################################################################

question <- function(bot, update){
  query <- update$callback_query
  
  
  measurement <- substr(query$data, 10, 10)
  
  if(exists(measurement)){
    measurement <- 0
  }
  
  print(measurement)
  

  text <- paste("Okay. ",
                "And how much fear can you locate in your body. Same scale again.", sep="")
  
  keyboard <- list(
    list(
      InlineKeyboardButton(
        text=0,
        callback_data=paste("result_0_", measurement, sep="")
      ),
      InlineKeyboardButton(
        text=1,
        callback_data=paste("result_1_", measurement, sep="")
      ),
      InlineKeyboardButton(
        text=2,
        callback_data=paste("result_2_", measurement, sep="")
      ),
      InlineKeyboardButton(
        text=3,
        callback_data=paste("result_3_", measurement, sep="")
      ),
      InlineKeyboardButton(
        text=4,
        callback_data=paste("result_4_", measurement, sep="")
      ),
      InlineKeyboardButton(
        text=5,
        callback_data=paste("result_5_", measurement, sep="")
      )
    )
  )
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keyboard
  )
  
  bot$sendMessage(chat_id = query$message$chat$id, text = text, reply_markup=IKM)
}


question_callback_handler <- CallbackQueryHandler(question, pattern = "question")
updater <- updater + question_callback_handler

######################################################################
# Result
######################################################################

result <- function(bot, update){
  query <- update$callback_query
  
  print(query$data)
  
  measurement1 <- as.numeric(substr(query$data, 10, 10))
  measurement2 <- as.numeric(substr(query$data, 8, 8))
  
  print("-------m1--m2--result:")
  print(measurement1)
  print(measurement2)
  
  measured_result <- measurement1 + measurement2
  print(measured_result)
  
  
  text <- paste("Thanks for sharing your state of mind. ",
                "I rate your sadness-level as: ", measured_result, "/10
                
",              "But what do I know. I'm just a bot. ", 
                "You could also speak, with my human friend Doktor Luigi, as well.",
                sep="")
  
  keyboard <- list(
    list(
      InlineKeyboardButton(
        text="Doktor Luigi",
        callback_data="doktorluigi"
      ),
      InlineKeyboardButton(
        text="Bye",
        callback_data="bye"
      )
    )
  )
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keyboard
  )
  
  bot$sendMessage(chat_id = query$message$chat$id, text = text, reply_markup=IKM)
}


result_callback_handler <- CallbackQueryHandler(result, pattern = "result")
updater <- updater + result_callback_handler

######################################################################
#I dont know
######################################################################

idontknow <- function(bot, chat_id)
{
  text <- paste("Its not easy to open up. No worries. ", 
            "And I know... I'm just a bot. Maybe you prefer to speek with ",
            "my good friend Doktor Luigi. I can send you his contact, if you feel like. ",
            "He's a real human, I guess. And dont't forget: ",
            "I'm here. So you can always talk to me. ", 
            "For now I leave you alone.", sep="")
  
  keyboard <- list(
    list(
      InlineKeyboardButton(
        text="Doktor Luigi",
        callback_data="doktorluigi"
      ),
      InlineKeyboardButton(
        text="Let's talk",
        callback_data="letstalk"
      ),
      InlineKeyboardButton(
        text="Bye",
        callback_data="bye"
      )
    )
  )
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keyboard
  )
  
  bot$sendMessage(chat_id = chat_id, text = text, reply_markup=IKM)
}

idontknow_command <- function(bot, update){
  idontknow(bot, update$message$chat_id)
}

idontknow_callback <- function(bot, update){
  query <- update$callback_query
  idontknow(bot, query$message$chat$id)
}

idontknow_command_handler <- CommandHandler("idontknow", idontknow_command)
idontknow_callback_handler <- CallbackQueryHandler(idontknow_callback, pattern = "idontknow")
updater <- updater + idontknow_command_handler + idontknow_callback_handler

######################################################################
#Bye
######################################################################

bye <- function(bot, chat_id, name)
{
  text <- paste("It's always a pleasure talking to you. ",
                "Enjoy your day, ", name, "!",
                sep="")
  bot$sendMessage(chat_id = chat_id,text)
}

bye_command <- function(bot, update){
  bye(bot, update$message$chat_id, update$message$from$first_name)
}

bye_callback <- function(bot, update){
  query <- update$callback_query
  bye(bot, query$message$chat$id, query$message$chat$first_name)
}


bye_command_handler <- CommandHandler("bye", bye_command)
bye_callback_handler <- CallbackQueryHandler(bye_callback, pattern = "bye")
updater <- updater + bye_command_handler + bye_callback_handler

######################################################################
#Doktor Luigi
######################################################################

doktorluigi <- function(bot, chat_id, name)
{
  text <- paste("Luigi is a good friend of mine. Such a sweety... ", 
                "You want his contact? Sure. ",
                "He's like me always open for little talks. ",
                "Just write him a message. He's there.
                
",              "Doktore Luigiano
",              "doktor.luigi@signsofsadness.world

",              "And if you want to talk to me, ", name, 
                ", I'm always here as well", sep="")
  
  keyboard <- list(
    list(
      InlineKeyboardButton(
        text="Let's Talk",
        callback_data="letstalk"
      ),
      InlineKeyboardButton(
        text="Bye",
        callback_data="bye"
      )
    )
  )
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keyboard
  )
  
  bot$sendMessage(chat_id = chat_id,text = text, reply_markup=IKM)
  
}

doktorluigi_command <- function(bot, update){
  doktorluigi(bot, update$message$chat_id, update$message$from$first_name)
}

doktorluigi_callback <- function(bot, update){
  query <- update$callback_query
  doktorluigi(bot, query$message$chat$id, query$message$chat$first_name)
}

doktorluigi_command_handler <- CommandHandler("doktorluigi", doktorluigi_command)
doktorluigi_callback_handler <- CallbackQueryHandler(doktorluigi_callback, pattern = "doktorluigi")
updater <- updater + doktorluigi_command_handler + doktorluigi_callback_handler

######################################################################
#Help
######################################################################

help_command <- function(bot, update)
{
  text <- paste("Asking for help is always good, ", update$message$from$first_name,
                ". You may try out following commands:
",      "/letstalk
",      "/idontknow
",      "/doktorluigi
",      "/plot
",      "/bye",sep="")
  bot$sendMessage(chat_id = update$message$chat_id, text = text)
}

help_command_handler <- CommandHandler("help", help_command)
updater <- updater + help_command_handler


######################################################################
#Default Answer
######################################################################

echo <- function(bot, update)
{
  text <- paste("Sadly I'm just a bot, I guess. ", 
                "So I can't understand all of your messages. However, I'm here to help. ",
                "If you want, we can have a little talk to share our thoughts and feelings.", sep="")
  
  keyboard <- list(
    list(
      InlineKeyboardButton(
        text="Let's Talk",
        callback_data="letstalk"
      ),
      InlineKeyboardButton(
        text="I don't know ...",
        callback_data="idontknow"
      )
    )
  )
  IKM <- InlineKeyboardMarkup(
    inline_keyboard = keyboard
  )
  
  #sendedMessage <- update$message$text
  
  bot$sendMessage(chat_id = update$message$chat_id, text = text, reply_markup=IKM)
}

#Nimmt alle Text-Nachrichten (es gäbe auch Bild-Updates und mehr)
echo_handler <- MessageHandler(echo, MessageFilters$text)
updater <- updater + echo_handler

######################################################################
#Plotting
######################################################################

Temperature <- airquality$Wind
jpeg(file="saving_plot3.jpeg")
hist(Temperature, col="blue")
dev.off()

plot_command <- function(bot, update)
{
  chat_id = update$message$chat_id
  text <- paste("Just a plot, to demonstrate R advantage, ", update$message$from$first_name,sep="")
  bot$sendMessage(chat_id = chat_id, text = text)
  
  photo_url <- paste(getwd(), "/saving_plot3.jpeg", sep="")
  {
    bot$sendPhoto(
      chat_id = chat_id,
      photo = photo_url,
    )
  }
}

plot_command_handler <- CommandHandler("plot", plot_command)
updater <- updater + plot_command_handler



######################################################################
#Start the Bot
######################################################################

updater$start_polling()
