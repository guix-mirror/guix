;; GNU Guix news, for use by 'guix pull'.
;;
;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "a98712785e0b042a290420fd74e5a4a5da4fc68f")
        (title (en "New @command{guix git authenticate} command")
               (de "Neuer Befehl @command{guix git authenticate}")
               (fr "Nouvelle commande @command{guix git authenticate}"))
        (body
         (en "The new @command{guix git authenticate} command authenticates a
Git repository by verifying commit signatures and ensuring they all come from
authorized parties, exactly like @command{guix pull} now does.

This command is primarily useful to developers of channels.  It allows them to
ensure, before pushing, that the channel only contains commits signed with
authorized keys.  But this command is also useful anytime you use Git and want
to allow people to authenticate code fetched from your repository.

Run @command{info \"(guix) Invoking guix git authenticate\"} for more info,
and see @uref{https://guix.gnu.org/blog/2020/securing-updates/} for details on
these mechanisms.")
         (de "Mit dem neuen Befehl @command{guix git authenticate} können Sie
ein Git-Repository authentifizieren. Dazu werden alle Commit-Signaturen
verifiziert und geprüft, dass jede von einer autorisierten Quelle kommt, genau
wie es @command{guix pull} nun tut.

Dieser Befehl hilft in erster Linie den Entwicklern von Kanälen. Mit ihm kann
vor einem Push sichergestellt werden, dass der Kanal nur Commits enthält, die
mit autorisierten Schlüsseln signiert worden sind. Aber der Befehl kann auch
helfen, wann immer Sie Git verwenden und ermöglichen wollen, dass Nutzer von
Ihrem Repository geladenen Code authentifizieren können.

Führen Sie @command{info \"(guix) Invoking guix git authenticate\"} aus, um
mehr Informationen zu erhalten, und lesen Sie
@uref{https://guix.gnu.org/blog/2020/securing-updates/} für die Details dieser
Mechanismen.")
         (fr "La nouvelle commande @command{guix git authenticate} authentifie
un dépôt Git en vérifiant les signatures sur les changements (@i{commits}) et
en s'assurant qu'elles sont autorisées, exactement comme @command{guix pull}
le fait désormais.

Cette commande est avant tout utile aux personnes développant un canal.  Elle
leur permet de s'assurer, avant de pousser des changements, que le canal ne
contient que des changements signés par des clefs autorisées.  Mais cette
commande peut aussi s'avérer utile dès que tu veux utiliser Git et permettre
aux autres d'authentifier le code récupéré depuis ton dépôt.

Lance @command{info \"(guix.fr) Invoking guix git authenticate\"} pour plus
d'informations.  Voir @uref{https://guix.gnu.org/blog/2020/securing-updates/}
pour en savoir plus sur ces mécanismes.")))

 (entry (commit "43badf261f4688c8a7a7a9004a4bff8acb205835")
        (title (en "@command{guix pull} authenticates channels")
               (de "@command{guix pull} authentifiziert Kanäle")
               (fr "@command{guix pull} authentifie les canaux"))
        (body
         (en "The @command{guix pull} and @command{guix time-machine} commands
now authenticate the source code that they pull, unless the new
@option{--disable-authentication} option is passed.  What this means is that
Guix ensures that each commit received is cryptographically signed by an
authorized developer.  This protects you from attempts to tamper with the Guix
repository and from attempts to ship malicious code to users.

This feature is currently limited to the @code{guix} channel but will soon be
available to third-party channel authors.")
         (de "Die Befehle @command{guix pull} und @command{guix time-machine}
prüfen nun die Authentizität des heruntergeladenen Quellcodes, außer wenn die
neue Befehlszeilenoption @option{--disable-authentication} angegeben
wurde. Das bedeutet, Guix stellt sicher, dass jeder empfangene Commit durch
einen autorisierten Entwickler kryptografisch signiert wurde. Das schützt Sie
vor Versuchen, das Guix-Repository zu manipulieren oder bösartigen Code an die
Nutzer auszuliefern.

Diese Funktionalität ist auf den @code{guix}-Kanal beschränkt, sie wird jedoch
bald auch Autoren dritter Kanäle zur Verfügung stehen.")
         (fr "Les commandes @command{guix pull} et @command{guix time-machine}
authentifient dorénavant le code source qu'elles obtiennent, à moins que la
nouvelle option @option{--disable-authentication} soit utilisée.  Cela
signifie que Guix s'assure que chaque soumission (@i{commit}) récupéré dispose
d'une signature cryptographique par un·e développeur·euse autorisé·e.  Cela te
protège de tentatives de modifications du dépôt Guix et de tentatives de
livrer du code malintentionné.

Cette fonctionnalité n'est actuellement disponible que pour le canal
@code{guix} mais le sera bientôt pour les canaux tiers.")))

 (entry (commit "c924e541390f9595d819edc33c19d979917c15ec")
        (title (en "@command{guix repl} adds support for running Guile scripts")
               (de "@command{guix repl} kann Guile-Skripte ausführen")
               (fr "@command{guix repl} permet d'exécuter des scripts en langage Guile"))
        (body
         (en "The @command{guix repl} command can now be used to run
Guile scripts.  Compared to just launching the @command{guile} command,
@command{guix repl} guarantees that all the Guix modules and all its
dependencies are available in the search path.  Scripts are run like this:

@example
guix repl -- my-script,scm --option1 --option2=option-arg arg1 arg2
@end example

Run @command{info \"(guix) Invoking guix repl\"} for more information.")
         (de "Der Befehl @command{guix repl} kann jetzt zur Ausführung von
Guile-Skripten verwendet werden.  Im Vergleich zum Befehl
@command{guile} garantiert @command{guix repl}, dass alle Guix-Module und
alle seine Abhängigkeiten im Suchpfad verfügbar sind.  Skripte werden wie
folgt ausgeführt:

@example
guix repl -- my-script,scm --option1 --option2 --option2=option-arg arg1 arg2
@end example

Weitere Informationen erhalten Sie mit
@command{info \"(guix.de) Aufruf von guix repl\"}.")
         (fr "La commande @command{guix repl} peut maintenant être utilisée
pour exécuter des scripts en langage Guile.  Par rapport au simple lancement
de la commande @command{guile}, @command{guix repl} garantit que tous les
modules Guix et toutes ses dépendances sont disponibles dans le chemin
de recherche.  Les scripts sont exécutés comme ceci :

@example
guix repl -- my-script,scm --option1 --option2=option-arg arg1 arg2
@end example

Exécutez @command{info \"(guix.fr) Invoquer guix repl\"} pour plus d'informations.")))

 (entry (commit "b460ba7992a0b4af2ddb5927dcf062784539ef7b")
        (title (en "Add support to boot from a Btrfs subvolume")
               (de "Unterstützung für Systemstart von einem
Btrfs-Unterlaufwerk hinzugefügt")
               (fr "Ajout du support pour démarrer depuis un sous-volume Btrfs")
               (nl "Nieuwe ondersteuning voor het opstarten vanaf een Btrfs-subvolume"))
        (body
         (en "The generation of the GRUB configuration file produced from an
operating system declaration now takes into account the use of a Btrfs
subvolume for the partition holding @file{/gnu/store}.  Run the command
@command{info \"(guix) Btrfs file system\"} for more information and
examples.")
         (de "Für die Erzeugung einer GRUB-Konfigurationsdatei aus einer
Betriebssystemdeklaration kann jetzt ein Btrfs-Unterlaufwerk („Subvolume“) für
die Partition mit @file{/gnu/store} angegeben werden.  Führen Sie
@command{info \"(guix) Btrfs file system\"} aus, wenn Sie mehr Informationen
und Beispiele sehen möchten.")
         (fr "La génération du fichier de configuration de GRUB produite à
partir de la déclaration d'un @code{operating-system} tient maintenant compte
de l'utilisation d'un sous-volume Btrfs pour la partition contenant
@file{/gnu/store}.  Exécutez la commande @command{info\"(guix) Btrfs file
system\"} pour des exemples et plus d'information.")
         (nl "Het opmaken van het GRUB-configuratiebestand op basis van
een @code{operating-system}-declaratie houdt nu rekening met het gebruik van
een Btrfs-subvolume voor de partitie die @file{/gnu/store} bevat.  Voer
@command{info \"(guix) Btrfs file system\"} uit voor meer informatie en
voorbeelden.")))

 (entry (commit "6456232164890dbf5aa20394ee24637feb4b7b9e")
        (title (en "@command{guix pack -RR} introduces a new execution
engine")
               (de "@command{guix pack -RR} führt neuen Ausführungstreiber
ein"))
        (body
         (en "The @command{guix pack -RR} command allows you to create a
tarball containing @dfn{relocatable binaries}.  Until now, those would rely
either on Linux ``unprivileged user namespaces'' or on PRoot, when
unprivileged user namespaces are not supported.  However, PRoot introduces
significant overhead for some workloads.

To address that, @command{guix pack -RR} introduces a third option based on an
extension to the GNU run-time linker (ld.so) and on Fakechroot, which incurs
very little overhead.  You can select the fastest option when executing a
relocatable binary like this:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Run @command{info \"(guix) Invoking guix pack\"} for more information.")
         (de "Mit dem Befehl @command{guix pack -RR} können Sie einen Tarball
mit @dfn{verschieblichen Binärdateien} erzeugen (englisch „Relocatable
Binaries“).  Bisher wurden diese entweder in „unprivilegierten
Benutzernamensräumen“ ohne Berechtigungen ausgeführt, oder in PRoot, wenn
keine unprivilegierten Benutzernamensräume unterstützt wurden.  Allerdings
fällt bei der Ausführung mit PRoot bei manchen Anwendungen deutlich mehr
Rechenaufwand an.

Um dem entgegenzuwirken, stellt @command{guix pack -RR} nun eine dritte Option
zur Verfügung, die sich eine Erweiterung des GNU-Laufzeit-Binders („Run-Time
Linker“, ld.so) und Fakechroot zu Nutze macht.  Dadurch entsteht fast kein
Mehraufwand.  Sie können sich die schnellste Option aussuchen, wenn Sie eine
verschiebliche Binärdatei ausführen, zum Beispiel so:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix pack\"} aus, wenn Sie
mehr wissen wollen.")))

 (entry (commit "88a96c568c47c97d05d883ada5afbc4e1200b10f")
        (title (en "New @option{--path} option for @command{guix graph}")
               (de "Neue Option @option{--path} für @command{guix graph}"))
        (body
         (en "The @command{guix graph} command has a new @option{--path}
option that instructs it to display the shortest path between two packages,
derivations, or store items.  For example, the command below displays the
shortest path from the @code{libreoffice} package to @code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Run @code{info \"(guix) Invoking guix graph\"} for more information.")
         (de "Der Befehl @command{guix graph} verfügt über eine neue
Befehlszeilenoption @option{--path}, die ihn den kürzesten Pfad zwischen zwei
Paketen, Ableitungen oder Store-Objekten ausgeben lässt.  Zum Beispiel zeigt
folgender Befehl den kürzesten Pfad vom Paket @code{libreoffice} zu
@code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Führen Sie @code{info \"(guix.de) Aufruf von guix graph\"} aus, um mehr zu
erfahren.")))

 (entry (commit "a33eac038a811603c8b9ed106ae405a5f80a0e9d")
        (title (en "GNU C Library upgraded")
               (de "GNU-C-Bibliothek aktualisiert")
               (es "Actualización de la biblioteca C de GNU")
               (fr "Mise à jour de la bibliothèque C de GNU")
               (nl "GNU C-bibliotheek bijgewerkt"))
        (body
         (en "The GNU C Library (glibc) has been upgraded to version 2.31.  To
run previously-installed programs linked against glibc 2.29, you need to
install locale data for version 2.29 in addition to locale data for 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

On Guix System, you can adjust the @code{locale-libcs} field of your
@code{operating-system} form.  Run @code{info \"(guix) Locales\"}, for more
info.")
         (de "Die GNU-C-Bibliothek (glibc) wurde auf Version 2.31
aktualisiert. Um zuvor installierte Programme, die an glibc 2.29 gebunden
worden sind, weiter benutzen zu können, müssen Sie Locale-Daten für Version
2.29 zusätzlich zu den Locale-Daten für 2.31 installieren:

@example
guix install glibc-locales glibc-locales-2.29
@end example

Auf Guix System genügt es, das @code{locale-libcs}-Feld Ihrer
@code{operating-system}-Form anzupassen. Führen Sie @code{info \"(guix.de)
Locales\"} aus, um weitere Informationen dazu zu erhalten.")
         (es "Se ha actualizado la biblioteca de C de GNU (glibc) a la versión
2.31. Para ejecutar programas instalados previamente que se encuentren
enlazados con glibc 2.29, es necesario que instale los datos de localización
de la versión 2.29 junto a los datos de localización de la versión 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

En el sistema Guix, puede ajustar el campo @code{locale-libcs} de su
declaración @code{operating-system}. Ejecute @code{info \"(guix.es)
Localizaciones\"} para obtener más información.")
         (fr "La bibliothèque C de GNU (glibc) a été mise à jour en version
2.31.  Pour pouvoir lancer tes programmes déjà installés et liés à glibc 2.29,
tu dois installer les données pour la version 2.29 en plus des données de
régionalisation pour la version 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

Sur le système Guix, tu peux ajuster le champ @code{locale-libcs} de ta forme
@code{operating-system}.  Lance @code{info \"(guix.fr) Régionalisation\"} pour
plus de détails.")
         (nl "De GNU C-bibliotheek (glibc) werd bijgewerkt naar versie 2.31.
Om gebruik te maken van reeds geïnstalleerde programma's die aan glibc 2.29
gebonden zijn, moet u de regionale informatie van versie 2.29 naast die van
versie 2.31 installeren:

@example
guix install glibc-locales glibc-locales-2.29
@end example

Op Guix System kunt u het @code{locale-libcs}-veld van uw
@code{operating-system}-vorm aanpassen.   Voer @code{info \"(guix) Locales\"}
uit voor verdere uitleg.")))

 (entry (commit "e1e6491226347d9fb93ff484d78cef98848a510a")
        (title (en "Guix Cookbook now available as Info")
               (de "Guix-Kochbuch jetzt als Info-Dokument verfügbar"))
        ;; TRANSLATORS: Adjust the URL and the 'info' command to refer to the
        ;; translated manual if it's available.
        (body (en "The new Guix Cookbook is now fetched by @command{guix pull}
and thus readily available in the Info format.  It aims to provide tutorials
and detailed examples covering a variety of use cases.  You can access it by
typing:

@example
info guix-cookbook
@end example

The Cookbook is currently available in English and German.  You can also find
it @uref{https://guix.gnu.org/cookbook/en/, on-line}.

Your contributions are welcome: @uref{https://guix.gnu.org/contact/, get in
touch with the developers} to share your recipes!")
              (de "Das neue Guix-Kochbuch wird nun von @command{guix pull}
geladen und steht dann im Info-Format zur Verfügung.  Darin sollen Anleitungen
und detaillierte Beispiele gezeigt werden, die eine breite Spanne an
Anwendungsfällen abdecken.  Um darauf zuzugreifen, geben Sie dies ein:

@example
info guix-cookbook.de
@end example

Das Kochbuch steht derzeit auf Deutsch und Englisch zur Verfügung.  Sie können
auch @uref{https://guix.gnu.org/cookbook/de/, online} darauf zugreifen.

Ihre Beiträge werden gerne gesehen.  Bitte
@uref{https://guix.gnu.org/contact/, kontaktieren Sie die Entwickler}, um Ihre
Rezepte mit uns zu teilen!")))

 (entry (commit "2ca7af43fe17d9acf082dce85d137a27a8ac4887")
        (title (en "Further reduced binary seed bootstrap")
               (de "Bootstrapping jetzt mit noch kleinerem Seed"))
        (body
         (en "The package graph on x86_64 and i686 is now rooted in a further
@dfn{reduced set of binary seeds}.  The initial set of binaries from which
packages are built now weighs in at approximately 60 MiB, a quarter of what it
used to be.  Run @code{info \"(guix) Bootstrapping\"} to learn more, or watch
the talk at @uref{https://fosdem.org/2020/schedule/event/gnumes/}.")
         (de "Der Paketgraph auf x86_64 und i686 hat jetzt eine noch
@dfn{kleinere Menge an binären Seeds} als Wurzel. Das heißt, die ursprüngliche
Menge an Binärdateien, aus denen heraus Pakete erstellt werden, machen nun
ungefähr 60 MiB aus, ein Viertel der früheren Größe. Führen Sie @code{info
\"(guix.de) Bootstrapping\"} aus, um mehr zu erfahren, oder schauen Sie sich
den Vortrag auf @uref{https://fosdem.org/2020/schedule/event/gnumes/} an.")))

 (entry (commit "0468455e7d279c89ea3ad1b51935efb2b785ec47")
        (title (en "Rottlog service added to @code{%base-services}")
               (de "Rottlog-Dienst ist nun Teil der @code{%base-services}"))
        (body (en "An instance of @code{rottlog-service-type}, the system
service responsible for log rotation, has been added to @code{%base-services}.
If your operating system configuration for Guix System is explicitly adding
@code{rottlog-service-type} to the services, you should now remove it.  See
the ``Log Rotation'' section of the manual for more information.")
              (de "Eine Instanz des @code{rottlog-service-type} für
Log-Rotation wurde zu den @code{%base-services} hinzugefügt.  Wenn der
Systemdienst bereits in Ihrer Konfiguration für Guix System ausdrücklich
genannt wurde, sollten Sie ihn jetzt daraus entfernen.  Siehe den Abschnitt
„Log-Rotation“ im Handbuch für weitere Informationen.")))

 (entry (commit "b6bee63bed4f013064c0d902e7c8b83ed7514ade")
        (title (en "@code{guile} package now refers to version 3.0")
               (de "Das @code{guile}-Paket bezeichnet jetzt Version 3.0"))
        (body (en "The @code{guile} package has been upgraded to version 3.0
 (instead of 2.2).  The @code{guile3.0-} packages have been renamed to their
original name, and @code{guile2.2-} variants of these packages have been
defined.  Additionally, derivations are now all built with Guile 3.0, and
system services also run on 3.0.")
              (de "Das @code{guile}-Paket wurde auf Version 3.0
 (statt 2.2) aktualisiert. Die Pakete, deren Namen mit @code{guile3.0-}
beginnen, wurden umbenannt, so dass sie nun den unveränderten Namen tragen,
während ihre Varianten mit @code{guile2.2-} hinzugefügt wurden.  Des Weiteren
werden jetzt alle Ableitungen mit Guile 3.0 erstellt und die Systemdienste
laufen auch auf 3.0.")))

 (entry (commit "e3e1a7ba08af2d58c47264c543617e499c239444")
        (title (en "@command{guix pull} now supports SSH authenticated
repositories")
               (de "@command{guix pull} unterstützt nun SSH-authentifizierte
Repositorys")
               (fr "@command{guix pull} prend maintenant en charge
l'authentification en SSH pour les dépôts.")
               (nl "@command{guix pull} ondersteunt nu SSH-geauthenticeerde
repository's."))
        (body (en "The @command{guix pull} command now supports SSH
authenticated repositories as argument of @option{--url} and in custom
channels definitions.  The authentication requires that an @command{ssh-agent}
is running.")
              (de "Der Befehl @command{guix pull} unterstützt nun über SSH
authentifizierte Repositorys als Argument von @option{--url} und in
selbstgeschriebenen Kanaldefinitionen. Zur Authentisierung muss ein
@command{ssh-agent} laufen.")
              (fr "La commande @command{guix pull} prend maintenant en
charge l'authentification SSH pour les dépôts dans l'argument @option{--url}
et dans le définitions de canaux personnalisés.  L'authentification
nécessite qu'un @command{ssh-agent} soit lancé.")
              (nl "Het @command{guix pull}-commando ondersteunt nu
SSH-geauthenticeerde opslag als argument na @option{--url} en bij het
schrijven van eigen kanaaldefinities.  Hiervoor moet een @command{ssh-agent}
gestart zijn.")))

 (entry (commit "8234fe653e61d0090138cbd4c48d877568355439")
        (title (en "Guix now runs on Guile 3.0")
               (de "Guix läuft jetzt auf Guile 3.0")
               (fr "Guix tourne maintenant sous Guile 3.0")
               (nl "Guix draait nu op Guile 3.0"))
        (body (en "The Guix revision you just pulled runs on version 3.0 of
GNU@tie{}Guile (previously it would run on version 2.2).  Guile 3.0 improves
performance through the use of just-in-time (JIT) native code generation.  The
switch should be entirely transparent to you.  See
@uref{https://gnu.org/software/guile} for more information on Guile 3.0.")
              (de "Die Guix-Version, die Sie gerade gepullt haben, läuft auf
Version 3.0 von GNU@tie{}Guile (und nicht mehr auf Version 2.2).  Guile 3.0
verbessert die Rechenleistung, indem native Maschinenbefehle „just in time“
erzeugt werden (JIT-Kompilierung).  Der Wechsel sollte für Sie völlig
transparent sein und Guix verhält sich gleich.  Siehe
@uref{https://gnu.org/software/guile} für weitere Informationen zu Guile
3.0.")
              (fr "La révision de Guix que tu viens de récupérer tourne sous
la version 3.0 de GNU@tie{}Guile (Guix tournait avant sous la version 2.2).
Guile 3.0 améliore la performance en générant du code natif à la volée (JIT).
Le changement devrait être totalement transparent pour toi.  Voir
@uref{https://gnu.org/software/guile} pour plus d'information sur Guile 3.0.")
              (nl "De Guix die u net heeft gepulld gebruikt versie 3.0 van
GNU@tie{}Guile (voorheen was dat versie 2.2).  Guile@tie{}3.0 draait dezelfde
programma's doorgaans sneller door ze ‘just-in-time’ (JIT) te vertalen naar
machine-instructies.  De omschakeling zou voor u volledig naadloos moeten
zijn.  Lees @uref{https://gnu.org/software/guile} voor meer informatie over
Guile@tie{}3.0.")))

 (entry (commit "828a39da68a9169ef1d9f9ff02a1c66b1bcbe884")
        (title (en "New @option{--diff} option for @command{guix challenge}")
               (de "Neue @option{--diff}-Option für @command{guix challenge}")
               (fr "Nouvelle option @option{--diff} sur @command{guix challenge}"))
        (body (en "The @command{guix challenge} command, which compares
binaries provided by different substitute servers as well as those built
locally, has a new @option{--diff} option.  With @option{--diff=simple} (the
default), @command{guix challenge} automatically downloads binaries and
reports the list of differing files; @option{--diff=diffoscope} instructs it
to pass them to @command{diffoscope}, which simplifies the comparison process.
Run @command{info \"(guix) Invoking guix challenge\"}, for more info.")
              (fr "La commande @command{guix challenge} qui compare les binaires
fournis par différents serveurs de substituts aux contsructions locales a une
nouvelle option @option{--diff}.  Avec @option{--diff=simple} (par défaut),
@command{guix challenge} télécharge automatiquement les binaires et rapporte
la liste des fichiers différents@tie{}; @option{--diff=diffoscope} lui dit
de les passer à @command{diffoscope} qui simplifie le processus de comparaison.
Lance @command{info \"(guix.fr) Invoquer guix challenge\"} pour plus d'info.")
              (de "Der Befehl @command{guix challenge}, mit dem Binärdateien
von unterschiedlichen Substitut-Servern oder lokale Erstellungen miteinander
verglichen werden können, hat eine neue Befehlszeilenoption @option{--diff}
bekommen.  Bei @option{--diff=simple} (der Voreinstellung) lädt @command{guix
challenge} automatisch Binärdateien herunter und listet sich unterscheidende
Dateien auf; wird @option{--diff=diffoscope} angegeben, werden sie an
@command{diffoscope} geschickt, was deren Vergleich erleichtert.  Führen Sie
@command{info \"(guix.de) Aufruf von guix challenge\"} aus, um nähere
Informationen zu erhalten.")))

 (entry (commit "f675f8dec73d02e319e607559ed2316c299ae8c7")
        (title (en "New command @command{guix time-machine}")
               (de "Neuer Befehl @command{guix time-machine}")
               (fr "Nouvelle commande @command{guix time-machine}"))
        (body (en "The new command @command{guix time-machine} facilitates
access to older or newer revisions of Guix than the one that is installed.
It can be used to install different versions of packages, and to
re-create computational environments exactly as used in the past.")
              (de "Der neue Befehl @command{guix time-machine} vereinfacht
den Zugriff auf ältere oder neuere Guix-Versionen als die installierte.
Er kann zur Installation bestimmter Paketversionen verwendet werden, aber
auch zur Wiederherstellung von Entwicklungsumgebungen, wie sie in der
Vergangenheit verwendet wurden.")
              (fr "La nouvelle commande @command{guix time-machine}
facilite l'accès à des versions antérieures ou postérieures par rapport
à la version installée.  Elle sert à installer des versions spécifiques
de paquets, ainsi à la restauration d'environnements dans un état
historique.")))
 (entry (commit "3e962e59d849e4300e447d94487684102d9d412e")
        (title (en "@command{guix graph} now supports package
transformations")
               (de "@command{guix graph} unterstützt nun Paketumwandlungen"))
        (body
         (en "The @command{guix graph} command now supports the common package
transformation options (see @command{info \"(guix) Package Transformation
Options\"}).  This is useful in particular to see the effect of the
@option{--with-input} dependency graph rewriting option.")
         (de "Der Befehl @command{guix graph} unterstützt nun die mit anderen
Befehlen gemeinsamen Umwandlungsoptionen (siehe @command{info \"(guix.de)
Paketumwandlungsoptionen\"}).  Sie helfen insbesondere dabei, die Wirkung der
Befehlszeilenoption @option{--with-input} zum Umschreiben des
Abhängigkeitsgraphen zu sehen.")
         (es "La orden @command{guix graph} ahora implementa las opciones
comunes de transformación de paquetes (véase @command{info \"(guix.es)
Opciones de transformación de paquetes\"}). Esto es particularmente
útil para comprobar el efecto de la opción de reescritura del grafo
de dependencias @option{--with-input}.")))

 (entry (commit "49af34cfac89d384c46269bfd9388b2c73b1220a")
        (title (en "@command{guix pull} now honors
@file{/etc/guix/channels.scm}")
               (de "@command{guix pull} berücksichtigt nun
@file{/etc/guix/channels.scm}")
               (es "Ahora @command{guix pull} tiene en cuenta
@file{/etc/guix/channels.scm}")
               (fr "@command{guix pull} lit maintenant
@file{/etc/guix/channels.scm}"))
        (body
         (en "The @command{guix pull} command will now read the
@file{/etc/guix/channels.scm} file if it exists and if the per-user
@file{~/.config/guix/channels.scm} is not present.  This allows administrators
of multi-user systems to define site-wide defaults.")
         (de "Der Befehl @command{guix pull} liest nun die Datei
@file{/etc/guix/channels.scm}, wenn sie existiert und es für den jeweiligen
Benutzer keine @file{~/.config/guix/channels.scm} gibt.  Dadurch können
Administratoren von Mehrbenutzersystemen systemweite Voreinstellungen
vornehmen.")
         (es "Ahora la orden @command{guix pull} lee el fichero
@file{/etc/guix/channels.scm} si existe y el fichero personalizable
@file{~/.config/guix/channels.scm} no está presente. Esto permite a quienes
administran sistemas con múltiples usuarias definir valores predeterminados
en el sistema.")
         (fr "La commande @command{guix pull} lira maintenant le fichier
@file{/etc/guix/channels.scm} s'il existe et si le fichier
@file{~/.config/guix/channels.scm} par utilisateur·rice n'est pas présent.
Cela permet aux personnes administrant des systèmes multi-utilisateurs de
définir les canaux par défaut.")))

 (entry (commit "81c580c8664bfeeb767e2c47ea343004e88223c7")
        (title (en "Insecure @file{/var/guix/profiles/per-user} permissions (CVE-2019-18192)")
               (de "Sicherheitslücke in @file{/var/guix/profiles/per-user}-Berechtigungen (CVE-2019-18192)")
               (es "Vulnerabilidad en los permisos de @file{/var/guix/profiles/per-user} (CVE-2019-18192)")
               (fr "Permissions laxistes pour @file{/var/guix/profiles/per-user} (CVE-2019-18192)")
               (nl "Onveilige @file{/var/guix/profiles/per-user}-rechten (CVE-2019-18192)"))
        (body
         (en "The default user profile, @file{~/.guix-profile}, points to
@file{/var/guix/profiles/per-user/$USER}.  Until now,
@file{/var/guix/profiles/per-user} was world-writable, allowing the
@command{guix} command to create the @code{$USER} sub-directory.

On a multi-user system, this allowed a malicious user to create and populate
that @code{$USER} sub-directory for another user that had not yet logged in.
Since @code{/var/@dots{}/$USER} is in @code{$PATH}, the target user could end
up running attacker-provided code.  See
@uref{https://issues.guix.gnu.org/issue/37744} for more information.

This is now fixed by letting @command{guix-daemon} create these directories on
behalf of users and removing the world-writable permissions on
@code{per-user}.  On multi-user systems, we recommend updating the daemon now.
To do that, run @code{sudo guix pull} if you're on a foreign distro, or run
@code{guix pull && sudo guix system reconfigure @dots{}} on Guix System.  In
both cases, make sure to restart the service afterwards, with @code{herd} or
@code{systemctl}.")
         (de "Das voreingestellte Benutzerprofil, @file{~/.guix-profile},
verweist auf @file{/var/guix/profiles/per-user/$USER}.  Bisher hatte jeder
Benutzer Schreibzugriff auf @file{/var/guix/profiles/per-user}, wodurch der
@command{guix}-Befehl berechtigt war, das Unterverzeichnis @code{$USER}
anzulegen.

Wenn mehrere Benutzer dasselbe System benutzen, kann ein böswilliger Benutzer
so das Unterverzeichnis @code{$USER} und Dateien darin für einen anderen
Benutzer anlegen, wenn sich dieser noch nie angemeldet hat.  Weil
@code{/var/…/$USER} auch in @code{$PATH} aufgeführt ist, kann der betroffene
Nutzer dazu gebracht werden, vom Angreifer vorgegebenen Code auszuführen.
Siehe @uref{https://issues.guix.gnu.org/issue/37744} für weitere
Informationen.

Der Fehler wurde nun behoben, indem @command{guix-daemon} diese Verzeichnisse
jetzt selbst anlegt statt das dem jeweiligen Benutzerkonto zu überlassen.  Der
Schreibzugriff auf @code{per-user} wird den Benutzern entzogen.  Für Systeme
mit mehreren Benutzern empfehlen wir, den Daemon jetzt zu aktualisieren.  Auf
einer Fremddistribution führen Sie dazu @code{sudo guix pull} aus; auf einem
Guix-System führen Sie @code{guix pull && sudo guix system reconfigure …}
aus.  Achten Sie in beiden Fällen darauf, den Dienst mit @code{herd} oder
@code{systemctl} neuzustarten.")
         (es "El perfil predeterminado de la usuaria, @file{~/.guix-profile},
apunta a @file{/var/guix/profiles/per-user/$USUARIA}.  Hasta ahora cualquiera
podía escribir en @file{/var/guix/profiles/per-user}, lo cual permitía
a la orden @command{guix} crear el subdirectorio @code{$USUARIA}.

En un sistema con múltiples usuarias, esto permitiría a cualquiera con
intención de causar daño crear ese subdirectorio @code{$USUARIA} con el nombre
de alguien que no hubiese ingresado en el sistema. Puesto que ese
subdirectorio @code{/var/@dots{}/$USUARIA} se encuentra en la ruta de binarios
predeterminada @code{$PATH}, el objetivo del ataque podría ejecutar código
proporcionado por la parte atacante. Véase
@uref{https://issues.guix.gnu.org/issue/37744} para obtener más información.

Se ha solucionando delegando en @command{guix-daemon} la creación de esos
directorios y eliminando los permisos de escritura para todo el mundo en
@code{per-user}. En sistemas con múltiples usuarias recomendamos actualizar
cuanto antes el daemon. Para hacerlo ejecute @code{sudo guix pull} si se
encuentra en una distribución distinta, o ejecute @code{guix pull && sudo guix system reconfigure @dots{}} en el sistema Guix. En ambos casos, asegurese de
reiniciar el servicio tras ello, con @code{herd} o @code{systemctl}.")
         (fr "Le profil utilisateur par défaut, @file{~/.guix-profile},
pointe vers @file{/var/guix/profiles/per-user/$USER}.  Jusqu'à
maintenant, @file{/var/guix/profiles/per-user} était disponible en
écriture pour tout le monde, ce qui permettait à la commande
@command{guix} de créér le sous-répertoire @code{$USER}.

Sur un système multi-utilisateur, cela permet à un utilisateur
malveillant de créer et de remplir le sous-répertoire @code{USER} pour
n'importe quel utilisateur qui ne s'est jamais connecté.  Comme
@code{/var/@dots{}/$USER} fait partie de @code{$PATH}, l'utilisateur
ciblé pouvait exécuter des programmes fournis par l'attaquant.  Voir
@uref{https://issues.guix.gnu.org/issue/37744} pour plus de détails.

Cela est maintenant corrigé en laissant à @command{guix-daemon} le soin
de créer ces répertoire pour le compte des utilisateurs et en
supprimant les permissions en écriture pour tout le monde sur
@code{per-user}.  Nous te recommandons de mettre à jour le démon
immédiatement.  Pour cela, lance @code{sudo guix pull} si tu es sur
une distro externe ou @code{guix pull && sudo guix system reconfigure
@dots{}} sur le système Guix.  Dans tous les cas, assure-toi ensuite de
redémarrer le service avec @code{herd} ou @code{systemctl}.")
         (nl "Het standaard gebruikersprofiel, @file{~/.guix-profile}, verwijst
naar @file{/var/guix/profiles/per-user/$USER}.  Tot op heden kon om het even wie
in @file{/var/guix/profiles/per-user} schrijven, wat het @command{guix}-commando
toestond de @code{$USER} submap aan te maken.

Op systemen met meerdere gebruikers kon hierdoor een kwaadaardige gebruiker een
@code{$USER} submap met inhoud aanmaken voor een andere gebruiker die nog niet
was ingelogd.  Omdat @code{/var/@dots{}/$USER} zich in @code{$PATH} bevindt,
kon het doelwit zo code uitvoeren die door de aanvaller zelf werd aangeleverd.
Zie @uref{https://issues.guix.gnu.org/issue/37744} voor meer informatie.

Dit probleem is nu verholpen: schrijven door iedereen in @code{per-user} is niet
meer toegestaan en @command{guix-daemon} maakt zelf submappen aan namens de
gebruiker.  Op systemen met meerdere gebruikers raden we aan om
@code{guix-daemon} nu bij te werken.  Op Guix System kan dit met
@code{guix pull && sudo guix system reconfigure @dots{}}, op andere distributies
met @code{sudo guix pull}.  Herstart vervolgens in beide gevallen
@code{guix-daemon} met @code{herd} of @code{systemctl}.")))

 (entry (commit "5f3f70391809f8791c55c05bd1646bc58508fa2c")
        (title (en "GNU C Library upgraded")
               (de "GNU-C-Bibliothek aktualisiert")
               (es "Actualización de la biblioteca C de GNU")
               (fr "Mise à jour de la bibliothèque C de GNU")
               (nl "GNU C-bibliotheek bijgewerkt"))
        (body
         (en "The GNU C Library (glibc) has been upgraded to version 2.29.  To
run previously-installed programs linked against glibc 2.28, you need to
install locale data for version 2.28 in addition to locale data for 2.29:

@example
guix install glibc-locales glibc-locales-2.28
@end example

On Guix System, you can adjust the @code{locale-libcs} field of your
@code{operating-system} form.  Run @code{info \"(guix) Locales\"}, for more
info.")
         (de "Die GNU-C-Bibliothek (glibc) wurde auf Version 2.29
aktualisiert. Um zuvor installierte Programme, die an glibc 2.28 gebunden
worden sind, weiter benutzen zu können, müssen Sie Locale-Daten für Version
2.28 zusätzlich zu den Locale-Daten für 2.29 installieren:

@example
guix install glibc-locales glibc-locales-2.28
@end example

Auf Guix System genügt es, das @code{locale-libcs}-Feld Ihrer
@code{operating-system}-Form anzupassen. Führen Sie @code{info \"(guix.de)
Locales\"} aus, um weitere Informationen dazu zu erhalten.")
         (es "Se ha actualizado la biblioteca de C de GNU (glibc) a la versión
2.29. Para ejecutar programas instalados previamente que se encuentren
enlazados con glibc 2.28, es necesario que instale los datos de localización
de la versión 2.28 junto a los datos de localización de la versión 2.29:

@example
guix install glibc-locales glibc-locales-2.28
@end example

En el sistema Guix, puede ajustar el campo @code{locale-libcs} de su
declaración @code{operating-system}. Ejecute @code{info \"(guix.es)
Localizaciones\"} para obtener más información.")
         (fr "La bibliothèque C de GNU (glibc) a été mise à jour en version
2.29.  Pour pouvoir lancer tes programmes déjà installés et liés à glibc 2.28,
tu dois installer les données pour la version 2.28 en plus des données de
régionalisation pour la version 2.29 :

@example
guix install glibc-locales glibc-locales-2.28
@end example

Sur le système Guix, tu peux ajuster le champ @code{locale-libcs} de ta forme
@code{operating-system}.  Lance @code{info \"(guix.fr) Régionalisation\"} pour
plus de détails.")
         (nl "De GNU C-bibliotheek (glibc) werd bijgewerkt naar versie 2.29.
Om gebruik te maken van reeds geïnstalleerde programma's die aan glibc 2.28
gebonden zijn, moet u de regionale informatie van versie 2.28 naast die van
versie 2.29 installeren:

@example
guix install glibc-locales glibc-locales-2.28
@end example

Op Guix System kunt u het @code{locale-libcs}-veld van uw
@code{operating-system}-vorm aanpassen.   Voer @code{info \"(guix) Locales\"}
uit voor verdere uitleg.")))
 (entry (commit "cdd3bcf03883d129581a79e6d6611b2afd3b277b")
        (title (en "New reduced binary seed bootstrap")
               (de "Neues Bootstrapping mit kleinerem Seed")
               (es "Nueva reducción de la semilla binaria para el lanzamiento inicial")
               (fr "Nouvel ensemble de binaires de bootstrap réduit")
               (nl "Nieuwe bootstrap met verkleinde binaire kiem"))
        (body
         (en "The package graph on x86_64 and i686 is now rooted in a
@dfn{reduced set of binary seeds}.  The initial set of binaries from which
packages are built now weighs in at approximately 130 MiB, half of what it
used to be.  Run @code{info \"(guix) Bootstrapping\"} to learn more, or watch
the talk at @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (de "Der Paketgraph auf x86_64 und i686 hat jetzt eine @dfn{kleinere
Menge an binären Seeds} als Wurzel. Das heißt, die ursprüngliche Menge an
Binärdateien, aus denen heraus Pakete erstellt werden, machen nun ungefähr
130 MiB aus, halb so viel wie früher. Führen Sie @code{info \"(guix.de)
Bootstrapping\"} aus, um mehr zu erfahren, oder schauen Sie sich den Vortrag
auf @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/} an.")
         (fr "Le graphe des paquets sur x86_64 et i686 prend maintenant sa
source dans un @dfn{ensemble réduit de binaires}.  L'ensemble initial des
binaires à partir desquels les paquets sont construits pèse maintenant environ
130 Mio, soit la moitié par rapport à l'ensemble précédent.  Tu peux lancer
@code{info \"(guix) Bootstrapping\"} pour plus de détails, ou regarder la
présentation sur @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (es "El grafo de paquetes en x86_64 y i686 ahora tiene su raíz en un
@dfn{conjunto reducido de semillas binarias}. El conjunto inicial de binarios
desde el que se construyen los paquetes ahora tiene un tamaño aproximado de
130_MiB , la mitad de su tamaño anterior. Ejecute @code{info \"(guix.es)
Lanzamiento inicial\"} para aprender más, o puede ver la charla en inglés
en@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (nl "Het netwerk van pakketten voor x86_64 en i686 is nu geworteld in
een @dfn{verkleinde verzameling van binaire kiemen}.  Die beginverzameling
van binaire bestanden waaruit pakketten gebouwd worden is nu zo'n 130 MiB
groot; nog maar half zo groot als voorheen.  Voer @code{info \"(guix)
Bootstrapping\"} uit voor meer details, of bekijk de presentatie op
@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")))

 (entry (commit "dcc90d15581189dbc30e201db2b807273d6484f0")
        (title (en "New channel news mechanism")
               (de "Neuer Mechanismus, um Neuigkeiten über Kanäle anzuzeigen.")
               (es "Nuevo mecanismo de noticias de los canales")
               (fr "Nouveau mécanisme d'information sur les canaux")
               (nl "Nieuw mechanisme voor nieuwsberichten per kanaal"))
        (body
         (en "You are reading this message through the new channel news
mechanism, congratulations!  This mechanism allows channel authors to provide
@dfn{news entries} that their users can view with @command{guix pull --news}.
Run @command{info \"(guix) Invoking guix pull\"} for more info.")
         (de "Sie lesen diese Meldung mit Hilfe des neuen Mechanismus, um
Neuigkeiten über Kanäle anzuzeigen — Glückwunsch! Mit diesem
Mechanismus können Kanalautoren Ihren Nutzern @dfn{Einträge zu
Neuigkeiten} mitteilen, die diese sich mit @command{guix pull --news}
anzeigen lassen können. Führen Sie @command{info \"(guix.de) Aufruf
von guix pull\"} aus, um weitere Informationen zu erhalten.")
         (es "Está leyendo este mensaje a través del mecanismo de noticias del
canal, ¡enhorabuena! Este mecanismo permite a las autoras de canales
proporcionar @dfn{entradas de noticias} que las usuarias pueden ver con
@command{guix pull --news}. Ejecute @command{info \"(guix.es) Invocación de
guix pull\"} para obtener más información.")
         (fr "Ce message t'arrive à travers le nouveau mécanisme d'information
des canaux, bravo !  Ce mécanisme permet aux auteur·rice·s de canaux de
fournir des informations qu'on peut visualiser avec @command{guix pull
--news}.  Tape @command{info \"(guix.fr) Invoquer guix pull\"} pour plus de
détails.")
         (nl "U leest dit bericht door een nieuw mechanisme om per kanaal
@dfn{nieuwsberichten} te verspreiden.  Proficiat!  Hiermee kunnen kanaalauteurs
mededelingen uitzenden die hun gebruikers met @command{guix pull --news} kunnen
lezen.  Voer @command{info \"(guix) Invoking guix pull\"} uit voor meer
informatie."))))
