rm(list=ls())
library(caret)
library(car)
library(dplyr)
library(RecordLinkage)
library(hunspell)

#check each word
check_word <- function(word){
  if (hunspell_check(word)==T){
    return (word)
  }
  else if (hunspell_check(word)==F){
    all <- hunspell_suggest(word) %>% unlist()
    candidates <- c(all[levenshteinSim(word, all) %>% which.max()],
                    all[jarowinkler(word, all) %>% which.max()],
                    all[levenshteinDist(word, all) %>% which.min()])
    
    # if any candidate value is missing, return original word
    if (is.na(candidates[1])==T || is.na(candidates[2])==T || is.na(candidates[3])==T){
      return (word)
    }
    
    # if at least two are the same, return the common word
    if (candidates[1]==candidates[2] || candidates[2]==candidates[3]){
      return (candidates[2])
    }
    else if (candidates[1]==candidates[3]){
      return (candidates[1])
    }
    else {
      return (word)
    }
  }
}

#use survey answers, which may be of more than one word
check_var <- function(var){
  out <-  gsub(" {2,}", "", var) %>% 
    strsplit(" ") %>% lapply(function(s){
      # if the word is missing, return NA
      if (length(s)==0){
        return (NA)
      }
      # if the word is non-missing, go head with below
      else if (length(s)>=1){
        # word gives the corrected first word
        word <- paste0(s[1] %>% substring(1,1)%>% toupper(), s[1] %>% substring(2,nchar(s[1])),sep="") %>% check_word()
        if (length(s)>1){
          # if not a single word, run the loop to combine the words
          for (i in 2:length(s)){
            frag <- paste0(s[i] %>% substring(1,1) %>% toupper(), 
                           s[i] %>% substring(2,nchar(s[i])),sep="") %>% check_word()
            word <- paste(word, frag, sep=" ")
          }
        }
        return (word)
      }}) %>% unlist()
  return (out)
}

#correct for misspelling
grouping <- function(var){
  out <- gsub("'", "", var) %>% #take out single quote sign
    recode("c('Anomalous', 'None', 'illegible', '-----------------------------', 'Na', 'Ingunna', 'Anything They Could Die From')=NA;
           
           c('Asma','Asthmas', 'Ashma')='Asthma';
           
           c('Breast Cancer', 'Cancer abreast', 'Cancer De Mama', 'Cancer De Moma',
           'Cancer Del Seno', 'Cancer e Sid a', 'Cancer En La Prostate', 'Cancer Seno',
           'Cancer Tod o Tip o', 'Colon Cancer', 'El Cacner', 'Pancreatic Cancer',
           'Prostate Cancer', 'Prostate Cancer R', 'Brain Tumor', 'El Cancer', 
           'Caner', 'Tumor','Tumors', 'Tumores', 'Carce', 'Lung Cancer', 'Pancreas Cancer', 
           'Prostate Cancer','Prostate Canter','Bone Cancer', 'Cancel', 'Canceler', 'Carce r', 'Vaginal Cancer',
           'Cancer De La Prostate', 'Cancer Hp', 'Cancer Prostate', 'Cancer Vaginal', 'Cervical Cancer')='Cancer';
           
           c('High Blood', 'High Blood P.', 'High blood Pressure', 'Pressure of Blood',
           'Pressure Of Blood', 'High Bloods', 'Blood Pressure heart Problem','High Blood Pressure', 
           'Blood Pressure', 'Low Blood Pressure', 'Low Pressure', 'Pressure','Hypertension','Hubcap','La Precision','Pres ion Alterable',
           'High B look Pressure', 'High blood', 'High Blood Heart', 'High Blood Treasure', 'Highbrow Blood Pressure','Pres ion','Press ion',
           'Hear Attack', 'Heart Attack', 'Heart','Heart Attacks', 'Heart Defects', 'Heart Des ease','Heart Diesel', 'A take De Corona', 'El Corona','Defamer Cerebral',
           'Heart Problems', 'Heart Disease', 'Heart a T tack','Heart attack', 'Heart Attach', 'Heart Clusters', 'Heart Cancer', 'Heart Clusters', 'Defame Cerebral',
           'Heart Diabetes', 'Heart Failure', 'Heart Minor', 'Heart Murmur', 'Heart Q', 'Hearty', 'Hearty Attack', 'Hebert Attack','Corona Statue','Defame','Defamer',
           'Cardiod Vascular', 'Cardiovascular Problems','Cardiovascular es', 'Cardiac Arrest','Statue Cardiac o', 'Statues Cardiacs','Attacker De Corona','Demeter Arterial',
           'Stroke', 'Strokes', 'Circulation', 'Arteries','Colts In Arteries', 'Hypertension Arterial', 'Precision Arteriole', 'Pres ion Arterial', 'Precision','President Alta',
           'Alta Pres ion', 'Pres ion Alta','Hp', 'Clogged Arteries', 'Chronic Schism Heart Disease', 'La Pres ion', 'Una Sop lo En El Corona', 'Alteration', 'Problem as De Circulatioin',
           'Precision Baja', 'Precision Alta', 'Precision Arterial', 'Precision Baja','Press ion Alta','Pres ion Aler o', 'Pres ion Corona', 'Pres ion Babita','Cad',
           'Pres ion Alta', 'Prestidigitation Alta', 'Status', 'Statue Al Corona', 'Statue Cardiacs', 'Statue Cerebral', 'enfermedad caldiaca', 'Pres ion Baja',
           'El Corona', 'Hobo', 'Infarct', 'Herat', 'President Alta', 'Coagulation', 'Coronary Disease', 'Hubcap', 'Triglyceride')='Cardiovascular';
           
           c('Blood leukemia', 'Euphemia', 'Zulema', 'Nehemiah', 'Suleiman')='Leukemia';
           c('High Sugar', 'High Blood Sugar', 'Car', 'Sugar', 'BloodSugar', 'Arrear', 'La Car', 'Car En La Sang re')='Blood Sugar';
           c('Cholesterol Alta', 'High Cholesterol', 'Cholesterol Alto', 'Alto Cholesterol', 'Sugar Cholesterol')='Cholesterol';
           c('Blood Sugar e Diabetes', 'Diabetes Type 2','La Diabetic', 'Diabetic', 'Diabetics', 'Diabetes Type Ii',
           'Diabetic Precision', 'Diabetic Sid a', 'Type 2 Diabetes', 'La Daveta', 'Diabetes q', 'Siamese')='Diabetes';
           c('Over Weight', 'Weight Lose', 'Obsidian', 'Weight', 'Overweight', 'Sobre Peso', 'Peso')='Obesity';
           
           c('Back Pain', 'Back Problem', 'Back Injury', 'Chest Pain', 'Chest Pains', 'Dolores Articular es', 'Dolores De P echo', 'Embryologist',
           'Chronic Pain', 'Lower Back Pain', 'Suffering From Head Injury', 'Knee Problems','Knee Problem', 'Muscular Ache', 'Ambrosial',
           'Knee Surgery', 'Back', 'Body Ache', 'Body Aches', 'Dolor En Los Hue sos', 'Fibrosis', 'Osteoporosis', 'Dolor De Cuprous', 
           'Problem a En La Columnar', 'Dolor De Espalda')='Body Pain';
           
           c('Psycho Disease','Anxiety','Stress', 'Nerves','Nerviness','Nerving','Nerving-depression','Nervousness','Depression', 
           'Neurotransmitters', 'Mental Disorders', 'Mental Illness', 'Mental Issues', 'Mental Problems', 'Bipolar', 'Instead', 
           'Postsdam', 'Schizophrenia', 'Statue')='Mental Health';
           
           c('Hiv', 'Hiv Aids', 'Hiv Hids','Aid', 'Sid a Hiv Aids', 'Vhf Hiv', 'Aids',
           'Handmaids', 'Seed a', 'Hiv-aids', 'Sid a', 'Vi h', 'Alacrity Sid a', 'El Sid a Hiv', 
           'std', 'Std', 'St ds', 'Stds', 'Syphilis', 'Cida', 'Hewie', 'Sid', 'Amidships', 'Mi u Aids', 'Sid a hive', 
           'Venereal Disease')='HIV-STD';
           
           c('Memory Loss', 'Memory', 'Lost Memory', 'Derrida De La Memorial')='Dementia';
           c('Albumen', 'Altimeter', 'Altimeters', 'Alzheimers', 'Alzheimers Disease', 'Al timers')='Alzheimer';
           c('Parkinson Disease', 'Parkinsons', 'Parthenon')='Parkinson';
           
           c('Hepatitis B', 'Hepatitis C', 'Hep B', 'Hepatics', 'Hp c')='Hepatitis';
           
           c('Allergen Seafood', 'Allergy Attack', 'Aspirin Allergies','Food Allergies', 'Allergies', 'Prophylaxis')='Allergy';
           c('Altruistic', 'Arthritides', 'Rheumatoid Arthritis')='Arthritis';
           c('Head','Headaches', 'Dolor De Caber', 'Dolores De Caber', 'Dolor De Caber Migrant', 'Migraine', 'Migrant')='Headache';
           
           c('Thyroid','Graves Disease Thyroid ism','Thyroid Disease', 'Thyroids','Tirades','Trodes', 'Thyroid e', 
           'Peroxides', 'Ti rode', 'T erodes', 'Tirade', 'Peroxides')='Thyroid Problems';
           c('Brain Issues', 'Oxygen To Brain', 'Brain', 'Brain Injury')='Brain Problems';
           c('Lung Disease', 'Lungs', 'Respiratory Failure', 'Respiratory', 'Lung Failure', 'Emphysema')='Lung Problems';
           c('Intestines', 'Crones Disease')='Intestinal Problems';
           c('By Pass Stomach', 'Stomach Ache', 'Stomach Pain', 'Stomaches Aches', 'Gastritis', 'Stomach',
           'Stomach Ulcer', 'Stomach Ulcers', 'Ulcers', 'Ulcer a', 'Ulcer', 'Ulster', 'Ulster')='Stomach Problems';
           c('Eye Sight','Losing Sight', 'Blindness', 'Vista', 'Glaucoma', 'Vision', 'Eye Problems', 
           'Eye Subproblems', 'Eyesight', 'Cetera', 'Cataracts', 'Ci ego')='Vision Problems';
           c('Kidney Des ease', 'Kidney Disease', 'Kidney Stones', 'Kidneys', 'Kidney Failure', 'Los Crinolines', 'Crinolines',
           'Eeriness', 'Kidney', 'Problem a De Eeriness', 'Renal')='Kidney Problems';
           c('Liver', 'Liver Disease', 'Liver Issues', 'Cirrhosis', 'Highroad', 'Problem a De Highroad', 'Sclerosis of Liver')='Liver Problems';
           c('Prostate', 'Prostate Issue')='Prostate Problems';
           c('Bone Issue')='Bone Problems';
           c('Skin', 'Skin Deases', 'Skin Issues', 'Saggy Skin', 'Skin Disease', 'Dermatitis', 'Psoriasis')='Skin Problems';
           'Pancreas'='Pancreas Problems';
           c('Appendicitis', 'Appendix')='Appendix Problems';
           c('Colitis', 'Colon')='Colon Problems';
           c('Inserion', 'Infection Leading To Amputation')='Infection';
           c('Corona', 'Statue De Crona')='Coronavirus';
           c('Ester odes', 'Steroids')='Steroid';
           c('High Fever', 'Rheumatic Fever', 'Firebreak Tito fortitudes', 'Firebreak Rheumatic a')='Fever';
           'Lu pas'='Lupus';
           c('P re-eclamsia', 'Precambrian')='Pre-eclampsia';
           c('Vesicular gall Bladder', 'Vesiular')='Gallbladder';
           c('Distrait Muscular','Muscular Ache', 'Muscle Fatigue')='Muscle Problem';
            
           'Loss Of Hearing'='Hearing Problems';
           'Acidosis'='Sarcoidosis';
           'Mb p'='MBP';
           'Leora'='Leprosy';
           'Harmonia'='Pneumonia';
           'Cele bro' = 'Cerebrovascular Disease';
           'Varistor'='Chicken Pox';
           'Lead Passioning'='Lead Poisoning';
           'Gripe'='Flu';
           'Epilepsy'='Seizure';
           'Hemorrhoids'='Hemorrhoid';
           'Epileptic'='Epilepsy';
           'Migraines'='Migraine';
           'Diatonic'='Dystonia';
           'Cloris'='Anemia';
           'Hearing'='Hearing Problems';
           'Viral'='Virus';
           'Chronic Diarrhea'='Diarrhea';
           'Hernias'='Hernia';
           'Quipster'='Cyst';
           'Fungus'='Fungal Infection';
           'Herpies'='Herpes';
           'tb'='Tuberculosis';")
  return (out)
}


#import data
hh <- as.data.frame(foreign::read.spss("~/Documents/RA/Wicer/HH_database 062215.sav")) %>%
  select(hworry_spcfy_1, hworry_spcfy_2, hworry_spcfy_3, phq30_sum) %>%
  sapply(tolower) %>% as.data.frame()

#hworry1
hh$hworry1_correct <- check_var(hh$hworry_spcfy_1) %>% grouping()
hh$hworry1_count <- lapply(strsplit(hh$hworry1_correct, " "), length) %>% unlist()
hh$hworry1_spell <- hunspell_check(hh$hworry1_correct)

hh$hworry1_correct %>% as.factor() %>% levels()
hh$hworry1_correct %>% as.factor() %>% summary()

#hworry2
hh$hworry2_correct <- check_var(hh$hworry_spcfy_2) %>% grouping()
hh$hworry2_count <- lapply(strsplit(hh$hworry2_correct, " "), length) %>% unlist()

hh$hworry2_correct %>% as.factor() %>% levels()
hh$hworry2_correct %>% as.factor() %>% summary()

#output
out <- select(hh, hworry1_correct, hworry2_correct) %>% na.omit()
#take the union of top 15 concerns in worry 1 and 2
top15 <- union(sort(out$hworry1_correct %>% as.factor() %>% summary(), decreasing=T)[1:15] %>% names(),
               sort(out$hworry2_correct %>% as.factor() %>% summary(), decreasing=T)[1:15]%>% names())
out$hworry1 <- out$hworry1_correct

#if not top 15, categorize as others
out$hworry1[is.na(match(out$hworry1_correct, top15))==T] <- "Others"
out$hworry2 <- out$hworry2_correct
out$hworry2[is.na(match(out$hworry2_correct, top15))==T] <- "Others"

out <- select(out, hworry1, hworry2) %>% apply(2, as.factor) %>% as.data.frame()
tab <- table(out$hworry1, out$hworry2)
write.csv(tab, file="/Users/yuelong/GitHub/health_concerns/data.csv")
