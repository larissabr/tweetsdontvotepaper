---
  title: "Brexit analysis"
author: "Larissa Peixoto Gomes"
date: "`r Sys.Date()`"
output:
  html_document:
  df_print: paged
pdf_document: default
word_document: default
---
  
  {r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)



# Codes used for analysis of Brexit tweets

##### Packages
require(tm)
require(topicmodels) 
require(ldatuning) 
require(stringr)
require(stm)
require(quanteda)
require(quanteda.textmodels)
require(quanteda.textplots)
require(quanteda.textstats)
require(quanteda.sentiment)
require(seededlda)
require(readtext)
require(ggplot2)
require(extrafont)
require(reshape2)
require(dplyr)
require(tidytext)
require(textdata)
require(tibble)
require(sentimentr)
require(lexicon)
require(data.table)
require(sjmisc)





##### Opening file, tokenising, creating corpus and dfm

twitter <- readtext("/twitter.csv",
                    ignore_missing_files = TRUE,
                    text_field = "text",
                    docid_field = "doc_id",
                    docvarsfrom = c("metadata", "filenames", "filepaths"),
                    dvsep = "_",
                    docvarnames = NULL,
                    encoding = "ASCII",
                    source = NULL,
                    cache = TRUE,
                    fill = TRUE,
                    verbosity = )

twitter$date <- str_split(twitter$created_at, ' ', simplify = TRUE)[,1]  
twitter$text1 <- gsub("#", "", twitter$text)  
twitter$text1 <- gsub("\\'s", "", twitter$text1) 

twitter_england <- subset(twitter, NATION=="England")

twitter_england <- subset(twitter_england, select = c(text1, date, theme))
twitter_england$doc_id <- 1:71684


twitter_wales <- subset(twitter, NATION=="Wales")

twitter_wales <- subset(twitter_wales, select = c(text1, date, theme))
twitter_wales$doc_id <- 1:4347

######################################################################

##### Frequency plot

freq <- twitter %>% 
  group_by(date, NATION, theme) %>%
  dplyr::summarise(sum = n()) 

freq <- freq %>% 
  group_by(NATION, theme) %>% 
  mutate(percent=sum/(sum(sum)))

freq$theme <- recode(freq$theme, "afghanistan"="Afghanistan",
                     "brexit" = "Brexit",
                     "rwanda" = "Rwanda",
                     "ukraine" = "Ukraine")

freq_plot <- freq %>% 
  ggplot(aes(as.Date(date), percent, group = NATION, colour = NATION)) +
  scale_colour_manual(values = c("#0B775E", "#F98400"))+
  geom_point() +
  geom_line()+
  ylab("")+xlab("")+
  facet_wrap(~theme, scales = "free")+
  scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b-%y")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

freq_plot + theme(legend.title = element_blank(),
                  plot.title = (element_text(size = 15, family = "Garamond", face = "bold")),
                  axis.title.y = element_text(hjust=0.5, color = "black", size = 22, family = "Garamond"),
                  axis.title.x = element_text(hjust=0.5, color = "black", size = 22, family = "Garamond"),
                  legend.text = element_text(hjust=1, color = "black", size = 24, family = "Garamond"),
                  axis.text.y = element_text(hjust=1, color = "black", size = 22, family = "Garamond"),
                  axis.text.x = element_text(hjust=1, color = "black", size = 22, family = "Garamond"),
                  strip.text = element_text(size = 24, family = "Garamond"))+
                  theme(legend.position = "bottom")

ggarrange(freq_afghanistan, freq_Brexit, freq_rwanda, freq_ukraine,  common.legend = TRUE, legend = "bottom")+
  annotate_figure(freq_plot, top = text_grob("Relative frequency of tweets per day per nation", 
                                             color = "black", face = "bold", size = 17, family = "Garamond",
                                             just = "right"))

######################################################################

##### Stopwords, sequences, words of interest

##### stopwords
long_stop <- sw_loughran_mcdonald_long
stop_smart <- as.list(get_stopwords(language = "en", source = "smart"))
stop_smart$lexicon <- NULL
stop_smart <- as.data.frame(stop_smart)

stopwords <- as.data.frame(long_stop) %>% 
  rename("word"="long_stop")
stopwords <- as.data.frame(stopwords)

stopwords <- rbind(stopwords,stop_smart)
stopwords <- unique(stopwords)
stopwords <- as.list(stopwords)


#### using tstat collocation to define sequences (tokens that should be analysed as one)
toks_eng_pre <- twitter_england %>% 
  subset(theme%in%"brexit") %>% 
  corpus(text_field = "text1") %>% 
  tolower() %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE, remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = c(stopwords("english"), stopwords,  "amp"), padding = FALSE) 


toks_wales_pre <- twitter_wales %>% 
  subset(theme%in%"brexit") %>% 
  corpus(text_field = "text1") %>% 
  tolower() %>%  
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE, remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = c(stopwords("english"), stopwords,"amp"), padding = FALSE) 



tstat_col_eng <- tokens_select(toks_eng_pre, pattern = "^[A-Z]", 
                               valuetype = "regex") %>% 
  textstat_collocations(min_count = 15, size = 2)

tstat_col_cym <- tokens_select(toks_wales_pre, pattern = "^[A-Z]", 
                               valuetype = "regex") %>% 
  textstat_collocations(min_count = 10, size = 2)

tstat_col_eng$nation <- "England"
tstat_col_cym$nation <- "Wales"
tstat_col <- full_join(tstat_col_cym, tstat_col_eng)



sequences <- tstat_col %>% 
  subset(z>5) %>% 
  subset(select = "collocation")


myseqs <- myseqs %>% 
  as.data.frame() 
myseqs <- myseqs %>% 
  rename("collocation" = ".")

sequences <- full_join(sequences, myseqs)

sequences <- unique(sequences)

sequences <- as.list(sequences)

######################################################################

###### Sequences (includes sequences qualitatively defined by author)

myseqs <- c("across england", "across europe", "afghan people","after brexit", "against iceland", "against russia",
            "against slovakia", "against wales", "another country", "anti immigration", "anti-brexit protest", "armed forces",
            "asylum claims", "asylum seeker", "asylum seekers", "attending schools", "author joseph", "avon canal", "backs brexit",
            "bad news", "bad thing", "bbc news", "beautiful country", "big ben", "black country", "boris johnson", "both sides",
            "brave people", "breaker remain", "breaking news", "bred welsh", "brexit brexit", "brexit campaign", "brexit campaigners",
            "brexit debate", "brexit vote", "brexit voter", "brexit voters",  "brighton england", "britain votes", "british airways",
            "british army", "british author", "british boxing", "british citizen", "british citizens", "british citizenship", "british council",
            "british electorate", "british empire", "british government", "british history",  "british isles", "british library",
            "british museum", "british passport", "british people",  "british politics", "british public", "british red", "british sports", 
            "british summer", "british summertime", "british troops",  "british tv", "british values", "british weather", "british wrestlers",
            "cameron resigns", "cardiff bay", "cash votes",  "champions league", "channel tunnel", "chick cocky", "church of england",
            "civil war", "claim asylum",  "climate change", "cmon england", "c'mon england", "cmon wales", "c'mon wales", "concentration camps",
            "congestion delays", "congestion moderate", "conservative party", "control immigration", "control over", "controlled immigration",
            "country back", "country club", "country house", "country mile", "country music", "country needs", "country park", 
            "country retreat", "country's future", "crisis appeal", "cross bred", "cup final", "customer service", "cymru am byth","daily mail",
            "dark day", "dark fantasy", "david beckham", "david cameron", "david cameron's", "davids day", "david's day", "delay m4",
            "delays possible", "demand uk", "democratic country", "dewi hapus",  "didn't know", "didn't realise", "didn't think", "didn't vote",
            "didn't want", "difference between", "different country", "diligence to", "dirty russian", "divided nation",
            "doc ukraine", "doesn't even", "doesn't matter", "doesn't mean", "doesn't want", "doing well", "donald trump", "done wales",
            "downing street", "drink outlet", "dual nationality", "due diligence", "dydd gwyl", "dydd gŵyl", "easter sunday", 
            "economic migrants", "eddie jones", "el brexit", "end up", "ended up", "england captain", "england fan", "england fans", 
            "england game", "england manager", "england match", "england play", "england rugby", "england shirt",
            "england squad", "england u.k", "england uk", "england's best", "english breakfast", "english channel",
            "english football", "english language", "english lit", "english literature", "english players", "english speaking",
            "english summer", "english teacher", "english teams", "english version", "english weather", "entire country", "especially when", 
            "et al", "ethnic minorities", "ethnic minority", "eu cash", "eu citizens", "eu countries", "eu does", "eu funding",
            "eu membership", "eu migration", "eu referendum", "european countries", "european country","european union", "eurovision song",
            "even worse",	"ever seen",	"every day",	"every game",	"every other",	"every single",	"every time",	"everyone else",	
            "everyone who",	"exactly what",	"expected to",	"fact that",	"fair play",	"fake news",	"fake revolt",	"falling apart",	
            "fantasy horror",	"far away",	"far more",	"far right",	"far too",	"feature shout",	"fed up",	"feel ashamed",	"feel like",	
            "feel sick",	"feel sorry",	"feels like",	"felt so",	"few days",	"few hours",	"few weeks",	"few years",	"find out",	
            "fingers crossed",	"finish top",	"finishing second",	"fintan o'toole",	"first day",	"first half",	"first place",	"first safe",
            "first time",	"flag profile",	"fleeing from",	"fleeing ukraine",	"fleeing war",	"fly zone",	"follow the",	"following brexit",
            "food poisoning",	"football fans",	"football team",	"for africans",	"for christmas",	"for decades",	"for example",	"for peace",
            "for processing",	"for sale",	"for tickets",	"for years",	"forced to",	"foreign policy",	"forget about",	"forward to",	
            "found out",	"fourth book",	"free movement",	"free preview",	"friday shout",	"frinton-on-sea england",	"from france",	
            "from london",	"from outside",	"from runcorn",	"ftt resets",	"fuck off",	"fuck sake",	"fucked up",	"full english",	"funny how",
            "future generations",	"game tonight",	"gareth bale",	"general election",	"genuine refugees",	"george osborne",	"george soros",
            "germany france",	"get away",	"get here",	"get knocked",	"get rid",	"get through",	"give him",	"give me",	"give them",	
            "give up",	"give us",	"glad i",	"glory to",	"go back",	"go down",	"go home",	"go through",	"god bless",	"god help",	
            "going through",	"going to",	"good enough",	"good friday",	"good idea",	"good job",	"good luck",	"good morning",	"good news",
            "good old",	"good reason",	"good thing",	"got money",	"government must",	"granted asylum",	"great again",	"great britain",
            "great british",	"great idea",	"great result",	"great start",	"great uncle",	"grew up",	"group b",	"grow up",	"gt gt",	
            "gwyl dewi",	"gŵyl dewi",	"ha ha",	"had enough",	"half time",	"happened to",	"happens next",	"happens now",	"happens when",
            "happy birthday",	"happy easter",	"happy independence",	"happy saint",	"happy st",	"hapus happy",	"has anyone",	"has changed",
            "has decided",	"has done",	"has fallen",	"has given",	"has gone",	"has happened",	"has lost",	"has made",	"has resigned",	
            "has spoken",	"has taken",	"has won",	"have already",	"have any",	"have changed",	"have decided",	"have destroyed",	
            "have donated",	"have done",	"have gone",	"have seen",	"have spoken",	"have taken",	"have tried",	"have turned",	
            "have watched",	"have woken",	"haven't got",	"haven't seen",	"having a",	"he did",	"he didn't",	"he does",	"he doesn't",	
            "he gets",	"he has",	"he lied",	"he needs",	"he plays",	"he said",	"he says",	"he should",	"he wants",	"he was",	"he won't",
            "help provide",	"help ukraine",	"help us",	"here's my",	"he's doing",	"hey twitter",	"higher than",	"hijacked to",	"him how",
            "his family",	"his mp's",	"his own",	"home immediately",	"home nation",	"home nations",	"home office",	"home secretary",	
            "hope you",	"hope you're",	"hoping this",	"horror story",	"how bad",	"how can",	"how dare",	"how did",	"how does",	"how far",
            "how long",	"how many",	"how much",	"huge anti-brexit",	"human rights",	"humanitarian appeal",	"illegal immigrant",	
            "illegal immigrants",	"illegal immigration",	"illegal migrants",	"i'm voting",	"immigration policy",	"immigration system",
            "independence day",	"independent country",	"independent nation",	"innocent civilians",	"innocent people",	"interest rates",
            "international law",	"invaded ukraine",	"invading ukraine",	"invasion of ukraine",	"irish passport",	"jeremy corbyn",	"jo cox",
            "joe allen",	"joe root",	"john oliver",	"johnson gove",	"join me",	"join us",	"jordan henderson",	"joseph roy",	"keep out",
            "kicked out",	"labour party",	"labour voters",	"lake district",	"last year",	"last years",	"law breaker",	"law breaking",	
            "le pen",	"leave because",	"leave campaign",	"leave campaigners",	"leave voters",	"leeds england",	"legal routes",	
            "legally binding",	"lib dems",	"little england",	"little englanders",	"local communities",	"local elections",	"london england",
            "london united",	"long term",	"long time",	"look after",	"look like",	"looking good",	"looks like",	"lunatics have",	"made me",
            "major tournament",	"make britain great again",	"mass immigration",	"mass migration",	"mental health",	"michael gove",	
            "middle england",	"miles away",	"million people",	"moderate delay",	"mp's back",	"mps demand",	"n ireland",	"narrow minded",	"nation divided",	"nation state",	"national anthem",	"near hungerford",	"new pm",	"new zealand",	"newport wales",	"next door",
            "next election",	"next few",	"next game",	"next general",	"next pm",	"next round",	"next time",	"next week",	"next year",
            "next years",	"nicola sturgeon",	"nigel farage",	"no chance",	"no confidence",	"no doubt",	"no fly",	"no idea",	"no longer",
            "no matter",	"no plan",	"no surprise",	"no way",	"no wonder",	"no words",	"no-fly zone",	"non eu",	"north east",	"north wales",
            "north west",	"northern ireland",	"northern irish",	"not allowed",	"not even",	"not illegal",	"not sure",	"not surprised",
            "nothing else",	"now clear",	"now keep",	"now let's",	"nuclear power",	"nuclear war",	"nuclear weapons",	"offshore camps",	
            "old cross",	"old people",	"older generation",	"ordinary people",	"other countries",	"other half",	"other mp's",	"other news",
            "other side",	"other than",	"other way",	"our borders",	"our children",	"our country",	"our country's",	"our economy",
            "our future",	"our lives",	"our own",	"our politicians",	"our sign",	"our wee",	"out dirty",	"over half",	"over here",	
            "over years",	"own country",	"own flag",	"own goal",	"park road",	"partygate this",	"paul mason",	"pay rise",	"people fleeing",
            "people saying",	"people smugglers",	"people traffickers",	"people who",	"performance from",	"person who",	"personally i",
            "petition via",	"pic what",	"pick up",	"picked up",	"piss poor",	"plan b",	"plan immediately",	"planning to",	"plans to",	
            "play iceland",	"played well",	"playing well",	"please checkout",	"please donate",	"please don't",	"please help",	
            "please share",	"please sign",	"please stop",	"please support",	"please tell",	"pleasure to",	"plus side",	"pm how",	
            "poland wales",	"political party",	"polling station",	"polling stations",	"port talbot",	"possesion shots",	"post brexit",
            "premier league",	"pretty much",	"pretty sure",	"preview buy",	"prime minister",	"prince of wales",	"priti patel",	
            "private schools",	"pro brexit",	"profile pic",	"project fear",	"protest at",	"proud to",	"provide aid",	"provide safe",	
            "public accept",	"public services",	"pull together",	"put up",	"putin apologist",	"quarter final",	"quarter finals",	
            "raise money",	"re brexit",	"reading england",	"red cross",	"redditch england",	"reduce immigration",	"referendum result",
            "referendum rules",	"referring to",	"refugee crisis",	"refugees arriving",	"refugees fleeing",	"refugees from",	"refuse to",
            "refused to",	"refuses to",	"refusing to",	"remain as",	"remain camp",	"remain campaign",	"remain voters",	"remember when",
            "reminder that",	"reminds me",	"resets uk",	"revolt working-class",	"right wing",	"rights record",	"robbie savage",	
            "roman abramovich",	"roy hodgson",	"roy wright",	"rugby team",	"rules triggering",	"russian army",	"russian children",	
            "russian forces",	"russian invasion",	"russian military",	"russian money",	"russian oligarchs",	"russian soldiers",	
            "russian troops",	"russia's invasion",	"rwanda deal",	"rwanda plan",	"rwanda policy",	"rwanda scheme",	"sad day",	
            "sad sad",	"sadiq khan",	"safe country",	"safe passage",	"safe routes",	"said he",	"saint david's",	"same old",	"same thing",
            "same time",	"same way",	"saturday night",	"save our",	"says he",	"says it",	"schools ramp",	"schools uk",	
            "scottish independence",	"second eu referendum",	"second half",	"second referendum",	"seek asylum",	"seeking asylum",
            "self serving",	"semi final",	"send asylum",	"send them",	"sending asylum",	"sending people",	"sending refugees",	"sent home",
            "set up",	"severe delay",	"severity moderate",	"shadow cabinet",	"shane warne",	"she said",	"she was",	"sheffield england",
            "short term",	"single market",	"sky news",	"slava ukraini",	"small minded",	"social media",	"song contest",	"south africa",	
            "south east",	"south wales",	"southampton england",	"sovereign nation",	"speak english",	"sports person",	"sri lanka",	
            "st davids",	"st david's",	"stay eu",	"still undecided",	"stop immigration",	"sturgeon says",	"suffering unbelievably",	"suggestions that",	"je suis european",	"survive partygate",	"take back control",	"take control",	"take over",	"taken our",
            "taken over",	"this country",	"those fleeing",	"tom daley",	"tomorrow night",	"tony blair",	"tory government",	"tory mps",	
            "tory party",	"totally agree",	"trade deal",	"trade deals",	"triple lifesaver",	"truly fucked",	"trust me",	"typical england",
            "uk economy",	"uk government",	"uk private",	"uk should",	"uk votes",	"ukraine crisis",	"ukraine flag",	"ukraine humanitarian",
            "ukraine invasion",	"ukraine refugees",	"ukraine sign",	"ukraine suffering",	"ukrainian people",	"ukrainian refugee",	
            "ukrainian refugees",	"ukranian refugees",	"uncontrolled immigration",	"undead rise",	"unimaginable cruelty",	"united kingdom",
            "vladimir putin",	"vote leave",	"vote remain",	"voted for",	"voted leave",	"voted out",	"voted remain",	"votes leave",	
            "voting leave",	"voting out",	"voting remain",	"wales game",	"wales match",	"wales northern",	"wales town",	"wales won",	
            "want to",	"war crime",	"war crimes",	"war criminal",	"warrington england",	"watching england",	"wee country",	"welsh assembly",
            "welsh business",	"welsh businesses",	"welsh cake",	"welsh cakes",	"welsh cob",	"welsh economy",	"welsh fans",	"welsh fire",
            "welsh flag",	"welsh football",	"welsh friends",	"welsh government",	"welsh independence",	"welsh language",	"welsh national",
            "welsh open",	"welsh rugby",	"west country",	"west wales",	"work together",	"workers rights",	"working class",	"working-class
culture",	"world class",	"world cup",	"world war",	"worried about",	"worry about",	"worse off",	"worse than",	"worst thing",
            "york england",	"young men",	"young people",	"your mp",	"crossing the channel",	"small boats",	"channel crossing",
            "channel crossings",	"helmand province",	"dominic raab",	"proudly serving",	"queen and country",	"june 23rd",	"23rd june",
            "better in")


###### words of interest
words <- c("immigra*","migra*", "asylum*", "refugee*", "xenophob*", "foreign*", "racis*", "ethnic*", 
           "muslim*", "hindu*", "indian*", "", "origin*", "country of origin",  "turban*", "towel*", 
           "tunic*", "hijab*", "veil*", "niqab*", "head cover", "religion", "tradition*",
           "white", "black", "brown", "paki", "asian",  "country", "countries", "nation*", "identit*", "cultur*", "slovakia", "iceland",  "poland", "polish", "ukrain*", "rwanda*", "pakistan*", "afghan*", "france", "french",
           "england", "english", "wales", "welsh", "british", "britain", "uk", "united kingdom", 
           "cymru", "byth", "scotland", "scottish", "united states", "united nations", "un",
           "eastern", "middle east*", "estate*", "nato", "north atlantic", "council*", "putin","oligarch*", "donation*", "communit*", "social hous*", "social care*", "welfare*", "business*",  "nhs", "national health service",
           "health*", "job*", "sanction*", "condemn*", "help*", "solidarity", "solidary",
           "£", "$", "€", "eu", "euro*", "media*", "news*", "report*", "elect*",  "campaign*", "brexit*", "work*", "burden*",  "cameron",
           "expat*", "visa", "crossing*", "small boat*", "channel cross*", "channel*", "deport*",
           "send back", "return", "kill*",  "boat*", "vot*", "right*", "right wing", "left*","left wing", "petition*",
           "government*", "peace*", "attack*", "unjust", "unfounded", "war", "crim*", "democrac*", "army", "armed forces", "local authorit*",
           "border*", "control*", "referendum*", "independen*", "prison*", "debate*", "public service*",
           "devolution", "devolved", "econom*", "russia*", "sturgeon", "public fund*", "avon",
           "trump", "biden",  "johnson", "patel", "drakeford", "labour", "tory", "tories", "minister*", "state", "partygate",
           "freedom*", "libert*", "autonom*","self-determination", "determination*",
           "shame*", "happy",  "sad*", "depress*", "protest*", "enough", "done", "tired", "reset*")


######################################################################

##### Brexit control features
##### This code includes words that appear often, rather than just known words of interest

dfm_control1 <- twitter_england %>%
  subset(theme%in%"brexit") %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_compound(myseqs) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords,"s", "+", "|", "amp"), padding = FALSE) %>% 
  dfm()

dfm_control2 <- twitter_wales %>%
  subset(theme%in%"brexit") %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_compound(myseqs) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords,"s", "+", "|", "amp"), padding = FALSE) %>% 
  dfm()


feat_control1 <- dfm_control1 %>% 
  topfeatures(200) %>% 
  as.data.frame()  

feat_control2 <- dfm_control2 %>% 
  topfeatures(200) %>% 
  as.data.frame()  


feat_control1 <- feat_control1 %>% 
  rownames_to_column() 


feat_control2 <- feat_control2 %>% 
  rownames_to_column()

controlwords <- full_join(feat_control1, feat_control2) %>% 
  subset(select="rowname") %>% 
  unique() %>% 
  as.list()


######################################################################


###### Comparing Brexit with other themes by nation

toks_eng <- twitter_england %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_keep(pattern = c(controlwords, sequences, words, negative, positive)) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords, "brexit-",  "brexit", "rwanda", "rwanda's", "afghanistan", "afghanistan's'", "ukraine","ukraine's",  "by", "it", "at", "you", "we", "he", "a", "an", "|", "amp", "&amp", "&"), padding = FALSE) 


dfm_englandt <- dfm(toks_eng)

toks_wales <- twitter_wales %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_keep(pattern = c(controlwords, myseqs, words, negative, positive)) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords, "brexit-",  "brexit", "rwanda", "rwanda's", "afghanistan", "afghanistan's'", "ukraine","ukraine's",  "by", "it", "at", "you", "we", "he", "a", "an", "|", "amp", "&amp", "&"), padding = FALSE) 


dfm_walest<-dfm(toks_wales)

######################################################################

##### Relative frequency analysis (keyness)
##### "Keyness" assigns scores to features that occur differentially across different categories

tstat_key_eng <- dfm_englandt %>%
  dfm_subset(theme%in%c("brexit", "ukraine")) %>% 
  dfm_group(groups = theme) %>% 
  textstat_keyness(target="brexit", correction = "default")


keyness_plot_eng <- textplot_keyness(tstat_key_eng,
                                     margin = 0.2,
                                     n = 15,
                                     font = "Garamond",
                                     labelsize=8,
                                     color = c("darkblue", "gray"))
tstat_key_cym <- dfm_walest %>% 
  dfm_subset(theme%in%c("brexit", "ukraine")) %>% 
  dfm_group(groups = theme) %>% 
  textstat_keyness(target="brexit", correction = "default")

keyness_plot_cym <- textplot_keyness(tstat_key_cym,
                                     margin = 0.2,
                                     n = 15,
                                     font = "Garamond",
                                     labelsize=8,
                                     color = c("darkblue", "gray"))

keyness_plot_eng

ggsave("keyness_plot_eng.jpg", height = 8)

keyness_plot_cym
ggsave("keyness_plot_cym.jpg", height = 8)


######################################################################


##### Analysis of Brexit only, comparing nations

twitter_eng_brexit<- twitter_england %>% 
  subset(theme%in%"brexit")

toks_eng <- twitter_eng_brexit %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_keep(pattern = c(controlwords, sequences, words, negative, positive)) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords, "brexit", "brexit'll", "brexits",  "brexit-", "brexit's",  "rwanda", "afghanistan", "afghanistan's'", "ukraine","ukraine's", "by", "it", "at", "you", "we", "he", "a", "an", "|", "amp", "&amp", "&"), padding = FALSE) 

dfm_england <- dfm(toks_eng)

twitter_cym_brexit <-twitter_wales %>% 
  subset(theme%in%"brexit") 
toks_wales <- twitter_cym_brexit %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_keep(pattern = c(controlwords, sequences, words, negative, positive)) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords, "brexit","brexit'll",  "brexit-",   "brexit's", "rwanda", "afghanistan", "afghanistan's'", "ukraine","ukraine's", "by", "it", "at", "you", "we", "he", "a", "an",  "|", "amp", "&amp", "&"), padding = FALSE) 


dfm_wales<-dfm(toks_wales)

toks_brexit <- twitter%>% 
  subset(theme%in%"brexit") %>% 
  corpus(text_field = "text1") %>% 
  tokens() %>%
  tokens(remove_punct = TRUE, remove_number = TRUE,  remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_keep(pattern = c(controlwords, sequences, words, negative, positive)) %>% 
  tokens_remove(pattern = c(stopwords("english"), stopwords, "brexit", "brexit'll", "brexit-", "brexit's", "rwanda", "afghanistan", "afghanistan's'", "ukraine", "ukraine's",  "by", "it", "at", "you", "we", "he", "a", "an", "|", "amp", "&amp", "&"), padding = FALSE) 


dfm_brexit <-dfm(toks_brexit)


tstat_key_brexit <- dfm_brexit %>% 
  dfm_group(groups = NATION) %>% 
  textstat_keyness(correction = "default") 

keyness_plot <- textplot_keyness(tstat_key_brexit,
                                 margin = 0.2,
                                 n = 15,
                                 font = "Garamond",
                                 labelsize=8,
                                 color = c("darkblue", "gray"))

keyness_plot

ggsave("keyness_plot_brexit.jpg", height = 8)

##### Saving all plots

keyness_plot <- annotate_figure(keyness_plot, top = text_grob("Text Keyness Brexit", face = "bold", size = 14, 
                                                              family = "Garamond"))

keyness_plot_cym <- annotate_figure(keyness_plot_cym, top = text_grob("Text Keyness Wales - Brexit & Ukraine", 
                                                                      face = "bold", size = 14, 
                                                                      family = "Garamond"))

keyness_plot_eng <- annotate_figure(keyness_plot_eng, top = text_grob("Text Keyness England - Brexit & Ukraine", 
                                                                      face = "bold", size = 14, 
                                                                      family = "Garamond"))

plotlist <- list(keyness_plot, keyness_plot_cym, keyness_plot_eng)
bottom_row <- ggarrange(keyness_plot_cym, keyness_plot_eng, ncol = 2)
top_row <- ggarrange(NULL, keyness_plot, NULL, ncol = 3, widths = c(1,2,1))

final_plot <- ggarrange(top_row, bottom_row, ncol = 1)

ggsave("keyness.jpg", height = 16, width = 15)



######################################################################


##### Feature co-occurrence matrix

set.seed(12345)


fcm_cym <- fcm(toks_wales, context = "window", tri = FALSE)
feat <- names(topfeatures(fcm_cym, 120))
options(ggrepel.max.overlaps = Inf)

texplot_cym <- fcm_select(fcm_cym, pattern = feat) %>%
  textplot_network(min_freq = 3, vertex_size = 1.5, edge_color = "#F98400" , vertex_labelsize = 7,
                   vertex_color = "black" , vertex_labelfont = "Garamond", omit_isolated = TRUE)
texplot_cym

ggsave(filename = "texplot_cym.jpg")

fcm_eng <- fcm(toks_eng, context = "window", tri = FALSE)
feat <- names(topfeatures(fcm_eng, 100))
options(ggrepel.max.overlaps = Inf)

texplot_eng <- fcm_select(fcm_eng, pattern = feat) %>%
  textplot_network(min_freq = 20, vertex_size = 1.5, edge_color = "#899DA4" , vertex_labelsize = 7,
                   vertex_color = "black" , vertex_labelfont = "Garamond", omit_isolated = TRUE)
texplot_eng

ggsave(filename = "texplot_eng.jpg")

######################################################################


##### Keywords-in-context analysis

tokens_cym <- twitter_cym_brexit %>% 
  corpus(text_field = "text1") %>% 
  tokens(remove_url=TRUE)

kwic_cym <- kwic(tokens_cym, c("race*", "*migrat*", "ethni*", "asylum*", "refugee*", "nhs", "£", "health*"), window=25)
kwic_cym$nation <- "Wales"

tokens_eng <- twitter_eng_brexit %>% 
  corpus(text_field = "text1") %>% 
  tokens(remove_url=TRUE)

kwic_eng <- kwic(tokens_eng, c("race*", "*migrat*", "ethni*", "asylum*", "refugee*", "nhs", "£", "health*"), window=25)
kwic_eng$nation <- "England"

kwic <- full_join(kwic_cym, kwic_eng)

kwic$tweet <- paste0(kwic$pre, " ", kwic$post)
kwic <- within(kwic,rm(pre, post, from, to))

fwrite(kwic, "focus.csv")

######################################################################

##### Topic-modelling 

##### Using stm

##### For Englang

dfm_trim_england <- dfm_england %>% 
  dfm_trim(min_termfreq = 7,
           min_docfreq = 3)


stm_england <- convert(dfm_trim_england,
                       to = "stm", docvars = docvars(dfm_trim_england))

documents <- stm_england$documents
vocab <- stm_england$vocab
data <- stm_england$meta

stm_object_england<- stm(documents = stm_england$documents,
                         vocab = stm_england$vocab,
                         data = stm_england$meta,
                         K = 15,
                         seed = 12345)


par(bty="n",col="grey40",lwd=1)

plot(stm_object_england, family = "Garamond")


plot(stm_object_england, type="labels", topics = c(2,8,11,12), labeltype = "frex", text.cex = 2, family= "Garamond")

###### This plot  tells us which topics are coming from which documents

plot(stm_object_england,type="hist", xlim = c(0, 0.3))


labelTopics(stm_object_england)

##### For Wales
dfm_trim_wales <- dfm_wales %>% 
  dfm_trim(min_termfreq = 5,
           min_docfreq = 2)


stm_wales <- convert(dfm_trim_wales,
                     to = "stm", docvars = docvars(dfm_trim_wales))

documents <- stm_wales$documents
vocab <- stm_wales$vocab
data <- stm_wales$meta

stm_object_wales<- stm(documents = stm_wales$documents,
                       vocab = stm_wales$vocab,
                       data = stm_wales$meta,
                       K = 10,
                       seed = 12345)

par(bty="n",col="grey40",lwd=1)

plot(stm_object_wales, family = "Garamond")


labelTopics(stm_object_wales)

processed <- textProcessor(twitter_brexit$text1, metadata=twitter_brexit,   removestopwords = TRUE,
                           removepunctuation = TRUE,
                           customstopwords = c("brexit", "brexit'll", "brexit-", "brexit's", "rwanda", 
                                               "afghanistan", "afghanistan's'", "ukraine",
                                               "ukraine's",  "by", "it", "at", "you", "we", "he", "a", "an", 
                                               "|", "amp", "&amp", "&"), stem = FALSE)


out_brexit <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs_b <- out_brexit$documents
vocab_b <- out_brexit$vocab
meta_b <- out_brexit$meta

stm_brexit <- stm(out_brexit$documents, out_brexit$vocab, K=15, prevalence=~NATION, 
                  data=out_brexit$meta, init.type="Spectral", 
                  seed=12345)


prep <- estimateEffect(1:15 ~ NATION, stm_brexit, meta=out_brexit$meta, 
                       uncertainty="Global")


plot(prep, covariate="NATION", c(2,3,4,5,6,7,8,10,11,14,15),  model=stm_brexit, 
     method="difference", cov.value1="England", cov.value2="Wales",
     xlab="More English ... More Welsh", main="Effect of England vs. Wales",
     xlim=c(-.05,.05))

plot(stm_object_brexit, family = "Garamond")

labelTopics(stm_object_brexit)


##### Using LDA

set.seed(12345)

tmod_lda_eng <- dfm(toks_eng) %>% 
  dfm_trim(min_termfreq = 0.7, termfreq_type = "quantile",
           max_docfreq = 0.3, docfreq_type = "prop") %>% 
  textmodel_lda(k = 15)

tmod_lda_cym <- dfm(toks_wales) %>% 
  dfm_trim(min_termfreq = 0.5, termfreq_type = "quantile",
           max_docfreq = 0.2, docfreq_type = "prop") %>% 
  textmodel_lda(k = 10)

topics_cym <- as.data.frame(topics(tmod_lda_cym, min_prob = 0.1))
topics_cym <- as.data.frame(topics_cym[complete.cases(topics_cym), ])

frq(topics_cym$`topics_cym[complete.cases(topics_cym), ]`, sort.frq = "desc")

topics_eng <- as.data.frame(topics(tmod_lda_eng, min_prob = 0.1))
topics_eng <- as.data.frame(topics_eng[complete.cases(topics_eng), ])
frq(topics_eng$`topics_eng[complete.cases(topics_eng), ]`, sort.frq = "desc")


termscym <- terms(tmod_lda_cym, 15)
fwrite(termscym, "termscym.csv")
termseng <- terms(tmod_lda_eng, 15)
fwrite(termseng, "termseng.csv")

thoughts_cym <- as.data.frame(findThoughts(tmod_lda_cym,texts=twitter_cym_brexit$text1, 
                                           topics=10, n=10)$docs[[1]])

thoughts_eng <- as.data.frame(findThoughts(tmod_lda_eng,texts=twitter_eng_brexit$text1, 
                                           topics=4, n=10)$docs[[1]])

fwrite(thoughts_cym, "thoughts_cym.csv")
fwrite(thoughts_eng, "thoughts_eng.csv")











