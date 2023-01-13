
# Text Adventure Game - The Gladiator Tournament

## Contents of the File

1. About the game
2. How to play and general idea of the structure of the game
3. About the files and functions

### About The Game

В тази игра , играча взема ролята на приключенец впускащ се в гладиаторски турнир за титлата Шампион на града ( и за парите разбира се). Играта е тип
текстови приключенска игра в която получавате информация за света чрез текст и взаймодействате със света чрез командите които въвеждате героя ви да направи.
Изборите в тази игра има значение за това дали ще стигнете крайната си цел затова четете внимателно.

### How to play and general idea

1. За да се компилира успешно играта е нужна инсталирана библиотеката System.Random. Изпълнява се командата ghc --make main.hs в директорията на играта и след това ./main ще стартира играта.

2. Генерална структура на проекта е ориентирана около дефинирания от мен data type World.

        data World = World
        {
            worldRooms :: [(EntityId Room,Room)],
            allItems :: [(EntityId Item,Item)],
            worldPeople :: [(EntityId Person,Person)],
            currentRoom :: (EntityId Room,Room),
            worldhero :: Hero
        }
        deriving (Eq,Show)

В една инстанция на този data type са събрани всички нужни инстанции на нужните ни типове данни. Другата важна част е newType EntityID a = EntityId Int

        newtype EntityId a = EntityId Int
        deriving (Eq,Show)

Обектите ни от тип Room , Person и Item притежават Int който служи за идентификатор в съответния му списък във World. Така когато работя с дадена стая,предмет или човек в нея не държа директно инстанциите от нужните ми типове а списъци от EntityId a които после използвам за да реферирам към списъците на World. Така избягвам евентуалното излишно създаване на инстанции на тип , които може евентуално вече да имам създадени. Ако обект не може да бъде намерен се връща defaultEntityId което е 0.

Също така дефинирам:

        data Command  = GoTo (EntityId Room) | Fight (EntityId Person) | LookAt (EntityId Person) | See (EntityId Item) | Drop (EntityId Item)
                | Use (EntityId Item)| History | Inventory  | DefaultCommand | Quit | Help 
        deriving (Eq,Show)

Чрез този тип управлявам какво се случва в играта. Играча въвежда команда която бива приемана като низ и подавана на ф-я(parseCommand) която преобразува низа в тип Command. След това командата бива подадена на друга ф-я(executeCommand) която в зависимост от командата извършва различни неща.

Типа WorldUpdateResult се използва за преглеждане на състоянието на играта след извършване на команда.

### About the files and functions

#### Всяка функция има кратък коментар над нея описващ какво прави

1. Main.hs - съдържа мейн функцията на проекта както и функциите fightLoop & specialEncounterLoop които след приключването си или терминират играта или се връщат към изпълнението на gameLoop
2. lib.hs - съдържа функциите боравещи с дефинираните от мен типове данни. Главните функции са parseCommand & executeCommand които от своя страна извикват нужните функции за изпълняването на команда.
3. types.hs - съдържа дефинираните от мен data types.
4. utils.hs - съдържа няколко по-малки функции които използвам повече от веднъж  в дефинициите на функциите от lib.hs
5. src.hs - Файл в който ръчно създавам инстанция на света , героя и стаите по които ще се движи. този файл после се импортва в main.hs и се ползва initialWorld.
    - descriptions.hs - Съдържа описанията на стаите и хората в играта.
    - entities.hs - Съдържа инстанциите на хората и предметите.




 