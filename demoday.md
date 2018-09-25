7 min 910 Words

# Einleitung 1.5 min

Hallo. Wir sind Steffi und Hannes von robur, und haben im Rahmen des Prototypefunds einen Kalenderserver geschrieben.

JedeR kennt das Problem: Man moechte sich verabreden, und alle sind beschaeftigt.  Wie wird ein fuer alle passender Termin gefunden? Ein gemeinsamer Kalendar muss her!

Dazu gibt es verschiedene Loesungen in der Cloud. Die Hardware ist abstrahiert und wird gemietet, um darauf zu rechnen und Daten zu speichern.
Was wir besonders wichtig finden ist digitale Selbstbestimmung ueber die eigenen Daten. Dazu ist wichtig, dass der Kalender auf eigener Infrastruktur, z.B. dem eigenen Server, betrieben werden kann, oder in der Cloud. Ausserdem sollte er einfach aufzusetzen und zu betreiben sein (bei Nextcloud kompliziert wegen Sicherheitsupdates). Daten sollen im Server sicher sein (bei nextcloud gab es Sicherheitsprobleme) und nicht an Datensammler weitergegeben werden (Google/Microsoft).
Unsere Loesung benutzt das DAV Protokoll, ein Kommunikationsprotokoll, dass das Web schreibbar macht.

Loesungen zum Selbsthosting sind komplex und bestehen aus der Anwendung selbst, der Laufzeitumgebung, dem Webserver, und dem unterliegenden Betriebssystem, und moeglicherweise anderen Diensten zum Betrieb des Servers. Jeder dieser Schichten muss einzeln konfiguriert werden, und Fehler koennen sich ueber die Schichten hinweg erstrecken und ein Fehler in einer einzelnen Schicht kann die komplette Anwendung unter die Kontrolle einer Angreiferin bringen.

Um die Gelegenheiten fuer Sicherheitsluecken und undurchsichtigen Code einzuschraenken, minimieren wir den Code des gesamten Systems in unserem Ansatz. Wir verzichten auf ein Allzweck-Betriebssystem und setzen auf einen Service, der massgeschneidert fuer genau einen Zweck ist. Bei uns ist dass das Erstellen und Verwalten von Kalendern.
Bei uns gibt es ueber der abstrahierten Hardware nur eine einzige Schicht: der Service ist das Betriebssystem.
Das Betriebssystem ist quasi garnicht da: MirageOS ist wie eine Fata Morgana.

Bisher war MirageOS ein Forschungsprojekt, in dem nuetzliche Software-Bibliotheken entwickelt wurden, aber nur beispielhafte Anwendungen.
Unser Kalenderprojekt ist eine der ersten praktischen Service-Anwendungen.
Da unser Kalendarprojekt mit allen Standard-Clients der grossen Betriebssysteme funktioniert, ist er fuer alle nutzbar.
Der Service ist aus modularen Software-Bibliotheken aufgebaut.
Er benutzt nur Bibliotheken, die absolut notwendig sind wie z.B. eine Netzwerkbibliothek und eine Uhr zur Datumsberechnung.

In unserem Projekt haben wir von Anfang an Techniken der sicheren Programmierung verwendet.
Hierzu zaehlt die funktionale Programmierung.
Die Wahl der Programmiersprache OCaml ermoeglicht uns, Klassen von Fehlern auszuschliessen.
OCaml hat ein sogenanntes Typsystem, mit dem ueberprueft werden kann, ob die eingegebenen Daten zur Anwendung passen.
So darf z.B. kein Fliesstext eingegeben werden, wenn eine Startzeit vom Kalender erwartet wird.
Daten von falscher Art fuehren zu einem Fehler, da nicht klar ist wie weiter gerechnet werden soll.
Anhand der Datentypen ist klar ersichtlich, mit welchen Eingaben das Programm arbeitet, und ob alle Eingabefaelle abgedeckt sind.
Eine haeufige Fehlerquelle ist, dass man bei einem laengeren Programm den Ueberblick verliert, was wo im Speicher abgelegt ist.
In OCaml wird dieses Problem umgangen, indem Speicher im Normalfall unveraenderbar belegt wird.
Z.B. kann man sich darauf verlassen, dass eine einmal belegte Variable immer auf den gleichen Wert verweist.
Programme koennen ohne Probleme kombiniert werden, da alle Variablenbelegungen sich nur lokal auswirken und es zu keinen Zusammenstoessen kommt.

# Hauptteil 4 min

Unser Prototyp besteht aus verschiedenen Komponenten.

Die Basis bildet das WebDAV-Protokoll vom schreibbaren Web und dessen Zugangskontrolle. Mit diesem Protokoll koennen Verzeichnisse, Dateien, und Dateieigenschaften am Server angelegt, veraendert, und geloescht werden.
Web-browsing im Internet basiert auf dem HTTP-Protokoll, bei dem ein Browser z.B. eine Webseite mit dem Verb GET und der Webadresse anfragt. Zu der Anfrage bekommt er eine Antwort mit dem Inhalt der Seite und einem Zahlen-Code, in dem Fehler ausgedrueckt werden kann (z.B. 404 - Not Found).
WebDAV erweitert das HTTP-Protokoll um zusaetzliche Verben, mit denen man Verzeichnisse und Dateieigenschaften abfragen und schreiben kann.
Die Eigenschaften werden im Protokoll per XML angefragt und zurueckgegeben, und bei uns auch als XML verwaltet.
Dazu haben wir eine Datenstruktur und Zugriffswerkzeuge entwickelt.

Wenn ich mir einen Termin merken will, schreibe ich ihn auf einen Klebezettel wie z.B. hier. Fuer den Computer gibt es ein anderes Format, dass icalendar-Format.  Darin koennen Kalender mit Zeitzonen, Events, und Todos abgelegt werden. Es ist auch moeglich, komplexe Zeitangaben zu machen, wie z.B. jeden Mittwoch, oder jeder letzte Sonntag im Monat.
Wie man hier sieht, besteht das Event aus Start, Dauer und Beschreibung. Andere Bestandteile sind auch moeglich, wie z.B. ein Alarm 5 Minuten vorher.
Wir haben eine Bibliothek entwickelt, die das icalendar Format ueberpruefen, lesen und ausgeben kann.

Fuer die Abwicklung einer HTTP-Anfrage nutzen wir die bestehende Bibliothek webmachine.  Sie implementiert ein Uebergangsmodell fuer HTTP, so dass man in der Anwendung nicht mit HTTP im Detail umzugehen braucht. Dieses Uebergangsmodell ist kompliziert, aber immer aehnlich und besteht aus einfachen ja/nein Fragen, die zur naechsten Frage oder einer Antwort mit Fehlercode fuehren. Die Anwendung ueberschreibt nur wenn noetig einzelne Fragen, was den Anwendungscode sehr aufgeraeumt macht.
Im Diagramm sieht man Entscheidungen, die bei der Bearbeitung einer Anfrage getroffen werden: zunaechst gibt es Validierung der Eingaben und Authentifizierung des Nutzers, dann wird verhandelt, ob ein passender Inhalt im passenden Format und Encoding zur Anfrage bereitgestellt wird.
Dann wird geschaut, ob die angefragte Resource (Webadresse) existiert oder es eine Umleitung gibt.
In bestimmten Faellen moechte man Anfragen nicht durchfuehren, z.B. wenn eine Resource zum Lesen angefragt wurde, die der Client in der gleichen Version bereits vorliegen hat. Oder wenn ein Schreiben einer Resource zu einem Konflikt fuehrt.  Diese Anfragen werden als bedingte Anfragen gesendet.
Als letztes wird die zum HTTP-Verb passende Aktion ausgefuehrt und eine Antwort gesendet.
Hier haben wir die Webmachine angepasst, um die zusaetzlichen WebDAV-Verben zu verarbeiten.

Der CalDAV-Server kombiniert die drei Bauteile mit etwas Verbindungscode.
Fuer CalDAV gibt es noch zusaetzliche HTTP-Verben zum Anlegen eines Kalenders und zur Abfrage mehrerer Events in einem Zeitraum.
Hier sieht man eine Beispielanfrage nach Events im Zeitraum vom 3. bis 5. Oktober.
Die Antwort enthaelt die passenden Events im Icalendar-Format.

TODO Adventure stories loeschen

TODO Beispielablauf (1.5m)
 * Erklaerung, dass wir jetzt gerade live testen in Marrakesh

# Schluss 1.5 min

Im Laufe des Projektes haben wir verschiedene Sachen gelernt.

Als Arbeitstechnik haben wir Pair-Programming gemacht, was Steffi von der Hacker School in New York mitgebracht hat. Taeglich wechselnder "tipper" an der Tastatur vs "driver" an der Entscheidungsfindung. Schwierige Entscheidungen wurden immer gemeinsam diskutiert und getroffen. Vorteil: Modell der Welt und Code Ownership sind in beiden Koepfen gleich :D
Kleine Hacks sammeln sich nicht an, da man sie gut begruenden muss. Die entstandene Software ist robust, da Entscheidungen und Kompromisse zwischen Aufwand und Genauigkeit bewusst getroffen und diskutiert werden.
Neue Anforderungen haben wir zunaechst auf der Ebene der Datentypen modelliert, und mit passenden Tests das Erfuellen der Spezifikation fuer die Zukunft garantiert.

Die tiefgreifendste Erkenntnis (fuer Steffi) war, dass das Betriebssystem keine komplizierte Blackbox sein muss.  Man kann hineinschauen und es verstehen - es ist auch nur Code wie jeder andere.
Eine Erkenntnis fuer uns beide war, dass Spezifikationen ueberraschende Fehler enthalten koennen, welche muehsam aus dem Anhang herausgesucht werden muessen.
Die Spezifikationen beschreiben eine Idee zu einem Zeitpunkt, reale Implementierungen weichen davon ab. Auch unsere Implementierung erlaubt z.B. nicht sekuendlich wiederholende Events, da dies rechenaufwaendig und fuer unsere Zwecke unrealistisch ist.
Die Berechnung von wiederholenden Terminen ist kompliziert, da Monate und Jahre verschieden lang sind, und Zeitzonen und Zeitumstellungen beachtet werden muessen.

TODO: ueberschrift "Zeitplanung" weg

Das groesste Problem waren unknown Unknowns, also Probleme die man im Vorhinein nicht wissen konnte. Im Nachhinein erscheinen diese einfach :D.
Dazu zaehlen Loecher in verwendeten Bibliotheken, die gestopft werden mussten, schwer ueberschaubare Menge an beteiligten Protokollen und deren Spezifikationen, uebereifrige Spezifikationen mit unrealistischen Szenarien, fehlende Informationen in den Spezifikationen.
Urspruenglich hatten wir mehr Projektteile geplant, wie ein In-Browser-Client, Protokolle fuer automatische Terminfindung und bessere Integration mit Google Calendar.
Uns freut, dass unser Kalender-Server mit Apple Calendar.app (iPhone, iPad, MacOS), Thunderbird Lightning, DavDroid (Android), und FirefoxOS gut funktioniert.
Ausserdem war das Pair-Programming super, und durch fortwaehrendes Testing der Einzelkomponenten wurden Rueckschritte ausgeschlossen.

Wir bedanken uns sehr, dass die Finanzierung das Projekt moeglich gemacht hat.
Es wird gerade auf unserem Workshop in Marrakesh getestet.
Ihr koennt es auch gern testen unter folgendem Link.
Wir hoffen, dass es Leuten inspiriert, sich ausserhalb von Google zu gemeinsamen Aktionen zu verabreden.

