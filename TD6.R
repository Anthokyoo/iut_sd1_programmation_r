#Exercice 1 
df <- read.csv(file = "nba2014_2015.csv", sep = ";",
               header = FALSE, dec = ";")
> nrow(nba)
> ncol(nba)
> colname(df)
> srt(df)
> df$Period <- as.factor(df$Period)
> df$PTSTYPE -> as.factor(df$PTSTYPE)
> df$SHOOTER = as.factor(df$shooter)




#1 Version corrigée
df = read.csv(file = "nba2014_2015.csv", sep = ",",
              header = TRUE, dec = ".") #Changer séparateur, header et décimal selon le fichier csv
nrow(df)
ncol(df) #nba n'existe pas, c'est df
colnames(df) #manquait un "s"
df$PERIOD = as.factor(df$PERIOD) #majuscule
df$PTS_TYPE = as.factor(df$PTS_TYPE) #tiret
df$SHOOTER = as.factor(df$SHOOTER) #majuscule


#Exercice 2 

length(levels(df$PERIOD)) 
length(df$PTS_TYPE)
length(df$SHOOTER)
summary(df)
sd(df$SHOT_DIST)
sd(df$SHOT_CLOCK, na.rm=TRUE)
     
     #combien de tirs manqués/réussis
     table(df[ "SHOT_RESULTS" , ])
     #les quartiles
     quantile(df$SHOT_CLOCK, probs = 4)
     #les déciles
     quantiles(df$CLOSE_DIST, probs = 10)
     #nombre de matches différents
     liste_game <- unique(df$GAME_ID))
length(listegame)
#nombre de joueurs différents
df$SHOOTER <- as_factor(df$SHOOTER)
nlevel(df$SHOOTER
       #conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
       nba$SHOT_DIST_METRE == SHOT_DIST * 0.30
       #nombre de points qu'a rapporté la tentative (0,2 ou 3)  
       df$PTS_MARQUES <- ifelse(df$SHOT_RESULT = "made", yes = df$PTS_TYPE, 0)
       #On supprime la variable GAME_RESULT car elle n'est pas utile
       df$GAME_RESULT <- NUL
       
       #création d'un objet sans la première colonne GAME_ID
       df2 <- df[ -1  ,  ]


#Execice 2 
       # Correction for the usage of length and levels (if 'Period' is a factor)
       length(levels(df$Period))
       
       # Correction for the length function usage
       length(df$PTSTYPE)
       length(df$SHOOTER)
       
       # Summary of the dataframe
       summary(df)
       
       # Standard deviation for SHOT_DIST and SHOT_CLOCK
       sd(df$SHOT_DIST)
       sd(df$SHOT_CLOCK)
       
       # Count of missed and made shots
       table(df$SHOT_RESULT)
       
       # Quartiles for SHOT_CLOCK
       quantile(df$SHOT_CLOCK, probs = seq(0, 1, by = 0.25))
       
       # Deciles for CLOSE_DIST
       quantile(df$CLOSE_DIST, probs = seq(0, 1, by = 0.1))
       
       # Number of different games
       length(unique(df$GAME_ID))
       
       # Convert SHOOTER to factor and calculate the number of levels (assuming SHOOTER is the correct column name)
       df$SHOOTER <- as.factor(df$SHOOTER)
       nlevels(df$SHOOTER)
       
       # Conversion of SHOT_DIST to meters
       df$SHOT_DIST_METRE <- df$SHOT_DIST * 0.3048
       
       # Points scored (0, 2, or 3) based on SHOT_RESULT and PTS_TYPE
       df$PTS_SCORED <- ifelse(df$SHOT_RESULT == 'made', df$PTS_TYPE, 0)
       
       # Remove the GAME_RESULT column if it's not needed
       df <- subset(df, select = -c(GAME_RESULT))
       
       # Creating a new dataframe without the first column (assuming GAME_ID is the first column)
       df2 <- df[,-1]
       
       
       
       
       #Exercice 3
       
       #Les 100 tirs réussis ou manqués les plus loin
       rang = order(df$SHOT_DIST, decreasing = TRUE)
       df3 <- df[, rang]
       df3 <- df[ 1 : 100 ; ]
       
       #Les 100 tirs réussis les plus loin
       df4 = subset(df3, SHOT_RESULT = made)
       df4 <- df[ 1 : 100 ; ]
       
       #Combien de tirs à 3 points a réussi Kobe Bryant ?
       df_kobe = subset(df,SHOT_RESULT = made &
                          PTS_TYPE = 3 & 
                          SHOOTER = "Kobe BRYANT")
       
       dim(df_kobe)
       
       #Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
       df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
       df_total_tri <- df_total[-order(df_total$PTS_MARQUES)]
       df_top5 <-  df_total_tri[  5  ,  ]
       
       Génère moi un fichier Excel en mettant chaque 

