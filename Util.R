# install.packages("telegram.bot")
library(telegram.bot)

bot <- Bot(token = bot_token("RTelegramBot"))

print(bot$getMe())

#erhalte alle nachrichten
updates <- bot$getUpdates()


#Sende einfache Nachricht
chat_id <- updates[[6]]$message$chat$id
bot$sendMessage(chat_id = chat_id, text = "Hello!")

updater <- Updater(token = bot_token("RTelegramBot"))

