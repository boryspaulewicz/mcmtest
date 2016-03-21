## -*- coding: utf-8 -*-

## Procedura: każda osoba ma oceniać i zapamiętać 30 przymiotników
## neg, neu i poz. Czas prezentacji każdego słowa jest stały.
##
if(interactive())source('~/cs/code/r/tasks/task/task.R')
TASK.NAME <<- 'mcmtest'

NOF.ITEMS = 10
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 5000

d = read.csv('slowa_final.csv')
neg = sample(tolower(as.character(d$słowo[(d$emocja == 'neg') & (d$rodzaj == 'p')])), NOF.ITEMS)
neu = sample(tolower(as.character(d$słowo[(d$emocja == 'neu') & (d$rodzaj == 'p')])), NOF.ITEMS)
pos = sample(tolower(as.character(d$słowo[(d$emocja == 'poz') & (d$rodzaj == 'p')])), NOF.ITEMS)

WINDOW$set.visible(T)
WINDOW$set.mouse.cursor.visible(T)

FX = fixation(WINDOW, size = .02)

scales = list(emotion = c('Znak emocji', 'Bardzo negatywne', 'Negatywne', 'Neutralne', 'Pozytywne', 'Bardzo pozytywne'),
              imagine = c('Wyobrażalność', 'Bardzo trudno', 'Trudno', 'Przeciętnie', 'Łatwo', 'Bardzo łatwo'),
              arousal = c('Pobudzenie', '1', '2', '3', '4', '5', '6', '7'))

## Test pamięciowy - ocena walencji ze stałym czasem ekspozycji
trial.code = function(trial, word = 'test', samegender = 'same', scale = 'emotion'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    ## Ewentualna zmiana genderu słowa
    if(((samegender == 'same') && (USER.DATA$gender == 'K')) ||
       ((samegender != 'same') && (USER.DATA$gender == 'M'))){
        word = str_replace_all(word, 'y$', 'a')
        word = str_replace_all(word, 'i$', 'a')
        if(word == 'mysa')word = 'mysia'
    }
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Proszę nacisnąć spację aby rozpocząć")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                scale.onset = CLOCK$time
                state = 'rating'
            }
        }, 'rating' = {
            WINDOW$clear(c(0, 0, 0))
            ## Rysujemy słowo
            TXT$set.string(word)
            center.win(TXT)## $move(c(0, WINDOW$get.size()[2] * -.2))
            WINDOW$draw(TXT)
            ## Pokazujemy skalę tylko dopóki nie zaznaczy odpowiedzi
            if(BUTTON.PRESSED[1] <= scale.onset){
                ## Pytanie dla skali (np. jak łatwo jest sobie wyobrazić...)
                TXT$set.string(scales[[as.character(scale)]][1])
                center.win(TXT)$move(c(0, WINDOW$get.size()[2] * .1))
                WINDOW$draw(TXT)
                value = draw.scale(scales[[as.character(scale)]][-1], position = .7)[1]
            }else{
                ## Słowo pokazujemy do końca czasu pokazywania słowa
                if((CLOCK$time - scale.onset) > PRESENTATION.TIME)state = 'done'
            }
            WINDOW$display()
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(rating = value)
            return(res)
        })
    }
}

gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Wyłącz telefon komórkowy.
W razie jakichkolwiek wątpliwości nie wołaj osoby prowadzącej, tylko podnieś do góry rękę.
Osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień. 
Badanie jest anonimowe.

Za chwilę zostaniesz poproszona/y o podanie danych: wieku, płci oraz pseudonimu.
Pseudonim składa się z inicjałów (małymi literami) oraz czterech cyfr:
dnia i miesiąca urodzenia (np.  ms0706).
")
gui.user.data()

cnd = 'same-emotion'
scale = str_split(cnd, '-')[[1]][2]

gui.show.instruction("
Teraz rozpocznie się zadanie wymagające zapamiętywania i oceny
słów. Na ekranie komputera będą się pojawiały, jedno po drugim,
różne słowa. Każde słowo będzie wyświetlane przez kilka sekund.

Należy zaznaczyć za pomocą myszki, przyciskając lewy
klawisz, na ile dane słowo kojarzy się negatywnie, neutralnie
lub pozytywnie.

Pozycja kursora przy ocenie słów ma znaczenie - pozycja skrajnie z
lewej strony oznacza maksymalnie negatywne skojarzenia, a pozycja
skrajnie z prawej strony - maksymalnie pozytywne skojarzenia.

Samo położenie kursora myszki nie wystarczy, należy jeszcze
potwierdzić ocenę klikając lewy przycisk myszki.

Należy starać się zapamiętywać wszystkie prezentowane słowa,
ponieważ na końcu badania będzie trzeba spróbować je odtworzyć z
pamięci.")

run.trials(trial.code, expand.grid(scale = scale,
                                   samegender = str_split(cnd, '-')[[1]][1],
                                   word = c(sample(neg), sample(neu), sample(pos))),
           record.session = F,
           condition = cnd)


######################################################################
## Zadanie dystrakcyjne - reagujemy lewo, prawo

## Globalne parametry zadania

MAX.REACTION.TIME = 3000
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000

## Globalne obiekty graficzne

TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW)
STIM = new(Text)
STIM$set.font(FONT)

## Funkcje pomocnicze, typu rysowanie bodźców

draw.stim = function(side){
    STIM$set.string(c(left = 'LEWO', right = 'PRAWO')[side])
    center.win(STIM)
    WINDOW$draw(STIM)
}

## Dwa klawisze w kluczu reakcyjnym

KEYS <<- c(Key.Left, Key.Right)

trial.code = function(trial, side = 'left'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Możliwość wyjścia z etapu za pomocą ESC
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            CORRECT.KEY <<- c(left = Key.Left, right = Key.Right)[side]
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - stim.onset) > MAX.REACTION.TIME))state = 'done'
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            return(list(rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - stim.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

TASK.NAME <<- 'leftright'
gui.show.instruction("
Teraz rozpocznie się zadanie wymagające szybkiego rozpoznawania słów.

Na środku ekranu będą się pojawiały słowa LEWO lub PRAWO.
Gdy tylko pojawi się słowo, należy nacisnąć odpowiednią strzałkę na klawiaturze.
Jeżeli będzie to słowo LEWO, należy nacisnąć klawisz STRZAŁKA W LEWO,
a jeżeli słowo PRAWO, to strzałkę STRZAŁKA W PRAWO.

Program będzie rejestrował zarówno czas reakcji, jak i
poprawność. Prosimy reagować możliwie szybko, ale poprawnie.

To zadanie potrwa około 3 minuty")

run.trials(trial.code, condition = 'default', record.session = F, expand.grid(side = c('left', 'right')),
           max.time = 5 * 60000, b = 3 * 60)

gui.show.instruction("
Prosimy teraz zapisać na kartce, z pamięci, w dowolnej kolejności,
słowa, które pojawiały się na ekranie w pierwszym zadaniu. Etap
odtwarzania słów będzie trwał około 3 minuty.

Po upłynięciu 3 minut od momentu naciśnięcia przycisku 'Dalej'
ekran zacznie migotać, aby zasygnalizować przejście do następnego
etapu.

Proszę nacisnąć przycisk 'Dalej' w dolnej części okna, aby rozpocząć
etap odtwarzania słów z pamięci.
")

WINDOW$set.visible(T)
recall.start = CLOCK$time
while((CLOCK$time - recall.start) < 3 * 60 * 1000){
    if(WINDOW$is.open())process.inputs()
    WINDOW$clear(c(0, 0, 0))
    WINDOW$display()
}
## Migotanie
blinking.start = CLOCK$time
TXT$set.string("Koniec odtwarzania. Naciśnij spację.")
center.win(TXT)
while(WINDOW$is.open()){
    process.inputs()
    if(KEY.PRESSED[Key.Space + 1] > blinking.start){
        break
    }else{
        col = ceiling(CLOCK$time / 1000) %% 2
        WINDOW$clear(c(col, col, col))
        WINDOW$draw(TXT)
        WINDOW$display()
    }
}

gui.show.instruction("
Teraz nastąpi kolejny etap zadania. Obok każdego słowa zapisanego
na kartce proszę zaznaczyć, na ile jesteś pewna/pewien, że to słowo
było (lub nie było) prezentowane wcześniej w zestawie do zapamiętania.

1 oznacza, że nie jesteś W OGÓLE pewna/pewny

2 oznacza, że jesteś NIEZBYT pewna/pewny

3 oznacza, że jesteś RACZEJ pewna/pewny

4 oznacza, że jesteś CAŁKOWICIE pewna/pewny

Po zakończeniu oceny słów proszę nacisnąć przycisk Dalej,
znajdujący się w dolnej części ekranu.
")



## Koniec
if(!interactive())quit("no")
