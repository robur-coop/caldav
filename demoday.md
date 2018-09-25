

Hello!!

Kalender, selbst-hosting, einfach zu betreiben, Was ist DAV? Kommunikationsprotokoll, kurz gesagt: Um das Web schreibbar zu machen.


Minimaler code fuer einen Service - wir sind das operating system (kein Linux / Windows / etc erforderlich).
Kein general purpose operating system (unsicher/komplex).


Service = als virtuelle maschine ausgefuehrt.
Schaubild: Herkoemmlicher Kalenderserver vs unsere Architektur - "Library Operating System"


Minimaler code fuer einen Service - wir sind das operating system (kein Linux / Windows / etc erforderlich).
Kein general purpose operating system (unsicher/komplex).


Komponenten die wir gebaut haben
Webdav + access control
DAV = Distributed Authoring and Versioning Protokoll
  * Properties-Datenstruktur (als XML format/anfragen) - Vorhaltesystem zum speichern der Eigenschaften einer Datei, z.B. zugriffsrechte, dateiformat, erstellungsdatum
  * Tree parser-kombinatoren auf XML (bequeme Zugriffsmoeglichkeit auf Daten in einer Baumstruktur).


- Icalendar-Dateiformat (parser und tooling)



- Nutzen Webmachine (Uebergangs-Modell fuer HTTP-Kommunikations-Protokoll, damit man nicht mit HTTP in der "business logic" / application logic umzugehen braucht und die applikation klarer wird.)
  * Erweiterung von Webmachine um DAV-Http-Verben (+ kurze erklaerung / demo?)


- Caldav (iCalendar ueber Webdav, + etwas glue, + implementierung zusaetzlicher verben z.B. MKCALENDAR, REPORT, Abfrage mehrerer events in einem Zeitraum)

Komponenten die wir gebaut haben
- In-Memory-Dateisystem
- Webdav + access control
  * Properties-Datenstruktur (als XML format/anfragen) - Vorhaltesystem zum speichern der Eigenschaften einer Datei, z.B. zugriffsrechte, dateiformat, erstellungsdatum
  * Tree parser-kombinatoren auf XML (bequeme Zugriffsmoeglichkeit auf Daten in einer Baumstruktur).
- Icalendar-Dateiformat (parser und tooling)
- Nutzen Webmachine (Uebergangs-Modell fuer HTTP-Kommunikations-Protokoll, damit man nicht mit HTTP in der "business logic" / application logic umzugehen braucht und die applikation klarer wird.)
  * Erweiterung von Webmachine um DAV-Http-Verben (+ kurze erklaerung / demo?)
- Caldav (iCalendar ueber Webdav, + etwas glue, + implementierung zusaetzlicher verben z.B. MKCALENDAR, REPORT, Abfrage mehrerer events in einem Zeitraum)
Adventure stories: Bugs in Webmachine, RFC fehler, irmin story, tree kombinatoren / XML parser


Komponenten die wir gebaut haben
- In-Memory-Dateisystem
- Webdav + access control
  * Properties-Datenstruktur (als XML format/anfragen) - Vorhaltesystem zum speichern der Eigenschaften einer Datei, z.B. zugriffsrechte, dateiformat, erstellungsdatum
  * Tree parser-kombinatoren auf XML (bequeme Zugriffsmoeglichkeit auf Daten in einer Baumstruktur).
- Icalendar-Dateiformat (parser und tooling)
- Nutzen Webmachine (Uebergangs-Modell fuer HTTP-Kommunikations-Protokoll, damit man nicht mit HTTP in der "business logic" / application logic umzugehen braucht und die applikation klarer wird.)
  * Erweiterung von Webmachine um DAV-Http-Verben (+ kurze erklaerung / demo?)
- Caldav (iCalendar ueber Webdav, + etwas glue, + implementierung zusaetzlicher verben z.B. MKCALENDAR, REPORT, Abfrage mehrerer events in einem Zeitraum)
Adventure stories: Bugs in Webmachine, RFC fehler, irmin story, tree kombinatoren / XML parser


Als Arbeitstechnik haben wir Pair Programming gemacht. Taeglich wechselnder “tipper” an der Tastatur vs “driver” an der Entscheidungsfindung. Schwierige Entscheidungen wurden immer gemeinsam diskutiert und getroffen. Vorteil: Modell der Welt und Code Ownership sind in beiden Koepfen gleich :D
Kleine Hacks sammeln sich nicht an, da man sie vor dem Mitprogrammierer begruenden muss. Die entstandene Software ist robust, da Designentscheidungen und Kompromisse zwischen Aufwand und Genauigkeit bewusst getroffen und diskutiert werden.

Neue Anforderungen haben wir zunaechst auf der Ebene der Datentypen modelliert, und mit passenden Tests das Erfuellen der Spezifikation fuer die Zukunft garantiert.



Was habt ihr auf dem Weg gelernt

Arbeitstechnische Erkenntnisse
* Wie war das Pair-Programming?
* Unit testing approach


Alles wird nur mit Wasser gekocht, "oeffnen von black boxes" = alle level, inklusive OS sind von Menschen gemacht und lesbar und muessen keine black box sein.

RFCs haben viele Errata.
Andere server und clients implementieren alles anders als der RFC :D

Rekurrente Kalenderevents sind nicht so einfach, Datumsberechnungen / Zeitzonen sind nicht so einfach.
Realistische Grenzen der implementierung (sekuendlich wiederholung ist selten :D).

Was haettet ihr anders gemacht, Fails and Wins

Hindsight bias: Projekt war zu gross, mussten Teile streichen (in-browser-client, sharing/scheduling-protokolle, interoperation mit google als client).

Kalender-Server funktioniert gut mit Apple Calendar.app (iPhone, iPad, MacOS), Thunderbird Lightning, DavDroid (Android), und FirefoxOS, und wird weiterhin live getestet.
Pair programming und unit testing waren ein win.



Was haettet ihr anders gemacht, Fails and Wins

Hindsight bias: Projekt war zu gross, mussten Teile streichen (in-browser-client, sharing/scheduling-protokolle, interoperation mit google als client).

Kalender-Server funktioniert gut mit Apple Calendar.app (iPhone, iPad, MacOS), Thunderbird Lightning, DavDroid (Android), und FirefoxOS, und wird weiterhin live getestet.
Pair programming und unit testing waren ein win.



Questions???
Thank you. :D