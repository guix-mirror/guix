;; GNU Guix news, for use by 'guix pull'.
;;
;; Copyright © 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;; Copyright © 2019, 2020 Miguel Ángel Arruga Vivas <rosen644835@gmail.com>
;; Copyright © 2019, 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;; Copyright © 2019, 2020, 2021 Julien Lepiller <julien@lepiller.eu>
;; Copyright © 2019, 2020, 2021 Florian Pelz <pelzflorian@pelzflorian.de>
;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;; Copyright © 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;; Copyright © 2021 Zhu Zihao <all_but_last@163.com>
;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "2161820ebbbab62a5ce76c9101ebaec54dc61586")
        (title
         (en "Risk of local privilege escalation during user account creation")
         (de "Risiko lokaler Rechteausweitung während der Erstellung von Benutzerkonten"))
        (body
         (en "A security vulnerability that can lead to local privilege
escalation has been found in the code that creates user accounts on Guix
System---Guix on other distros is unaffected.  The system is only vulnerable
during the activation of user accounts that do not already exist.

This bug is fixed and Guix System users are advised to upgrade their system,
with a command along the lines of:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

The attack can happen when @command{guix system reconfigure} is running.
Running @command{guix system reconfigure} can trigger the creation of new user
accounts if the configuration specifies new accounts.  If a user whose account
is being created manages to log in after the account has been created but
before ``skeleton files'' copied to its home directory have the right
ownership, they may, by creating an appropriately-named symbolic link in the
home directory pointing to a sensitive file, such as @file{/etc/shadow}, get
root privileges.

See @uref{https://issues.guix.gnu.org/47584} for more information on this
bug.")
         (de "Eine Sicherheitslücke, die eine lokale Rechteausweitung zur
Folge haben kann, wurde in dem Code gefunden, mit dem Benutzerkonten auf Guix
System angelegt werden — Guix auf anderen Distributionen ist nicht betroffen.
Das System kann nur während der Aktivierung noch nicht existierender
Benutzerkonten angegriffen werden.

Der Fehler wurde behoben und wir empfehlen Nutzern von Guix System, ihre
Systeme zu aktualisieren, mit einem Befehl wie:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Der Angriff kann erfolgen, während @command{guix system reconfigure} läuft.
Wenn @command{guix system reconfigure} ausgeführt wird, kann das die Erzeugung
neuer Benutzerkonten auslösen, wenn in der Konfiguration neue Konten angegeben
wurden.  Wenn ein Benutzer, dessen Konto gerade angelegt wird, es
fertigbringt, sich anzumelden, bevor „Skeleton-Dateien“ in seinem Persönlichen
Verzeichnis den richtigen Besitzer haben, kann er durch Anlegen einer gezielt
benannten symbolischen Verknüpfung in seinem Persönlichen Verzeichnis auf eine
sensible Datei wie @file{/etc/shadow} Administratorrechte erlangen.

Siehe @uref{https://issues.guix.gnu.org/47584} für mehr Informationen zu
diesem Fehler.")))

 (entry (commit "e52ec6c64a17a99ae4bb6ff02309067499915b06")
        (title
         (en "New supported platform: powerpc64le-linux")
         (de "Neue Plattform wird unterstützt: powerpc64le-linux")
         (fr "Nouvelle plate-forme prise en charge : powerpc64le-linux"))
        (body
         (en "A new platform, powerpc64le-linux, has been added for
little-endian 64-bit Power ISA processors using the Linux-Libre kernel.  This
includes POWER9 systems such as the
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
RYF Talos II mainboard}. This platform is available as a \"technology
preview\": although it is supported, substitutes are not yet available from
the build farm, and some packages may fail to build.  In addition, Guix System
is not yet available on this platform.  That said, the Guix community is
actively working on improving this support, and now is a great time to try it
and get involved!")
         (de "Eine neue Plattform, powerpc64le-linux, wurde hinzugefügt. Mit
ihr können Prozessoren mit 64-Bit-Power-Befehlssatz, little-endian, mit dem
Linux-Libre-Kernel betrieben werden.  Dazu gehören POWER9-Systeme wie die
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
RYF-zertifizierte Talos-II-Hauptplatine}.  Bei der Plattform handelt es sich
um eine „Technologievorschau“; obwohl sie unterstützt wird, gibt es noch keine
Substitute von der Erstellungsfarm und bei manchen Paketen könnte die
Erstellung fehlschlagen.  Des Weiteren ist Guix System auf dieser Plattform
noch nicht verfügbar.  Dennoch arbeitet die Guix-Gemeinde aktiv daran, diese
Unterstützung auszubauen, und jetzt ist eine gute Gelegenheit, sie
auszuprobieren und mitzumachen!")
         (fr "Une nouvelle plate-forme, powerpc64le-linux, a été ajoutée pour
les processeurs POWER 64-bits utilisant le noyau Linux-libre.  Ça inclut les
systèmes POWER9 tels que les
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
cartes Talos II RYF}.  Il s'agit pour le moment d'un « avant-goût » de la
technologie : bien que la plate-forme soit prise en charge, la ferme de
compilation ne fournit pas encore de substituts et certains paquets risquent
de ne pas compiler.  En outre, Guix System n'est pas encore disponible sur
cette plate-forme.  Ceci dit, la communauté Guix travaille activement pour
améliorer cette prise en charge et c'est maintenant un bon moment pour
l'essayer et pour s'impliquer !")))

 (entry (commit "9ade2b720af91acecf76278b4d9b99ace406781e")
        (title
         (en "Update on previous @command{guix-daemon} local privilege escalation")
         (de "Nachtrag zur lokalen Rechteausweitung bei @command{guix-daemon}")
         (nl "Aanvulling bij escalatie van bevoegdheden via @command{guix-daemon}"))
        (body
         (en "The previous news item described a potential local privilege
escalation in @command{guix-daemon}, and claimed that systems with the Linux
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''} feature enabled were unaffected by the vulnerability.

This is not entirely correct.  Exploiting the bug on such systems is harder,
but not impossible.  To avoid unpleasant surprises, all users are advised to
upgrade @command{guix-daemon}.  Run @command{info \"(guix) Upgrading Guix\"}
for info on how to do that.  See
@uref{https://guix.gnu.org/en/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
for more information on this bug.")
         (de "In der letzten Neuigkeit wurde eine mögliche lokale
Rechteausweitung im @command{guix-daemon} beschrieben und behauptet, dass
Systeme, auf denen Linux’
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
„Geschützte-Hardlinks“-Funktionalität} aktiviert ist, von der Sicherheitslücke
nicht betroffen seien.

Das stimmt nicht ganz.  Die Lücke auf solchen Systemen auszunutzen, ist
schwerer, aber nicht unmöglich.  Um unangenehme Überraschungen zu vermeiden,
empfehlen wir allen Nutzern, @command{guix-daemon} zu aktualisieren.  Führen
Sie @command{info \"(guix.de) Aktualisieren von Guix\"} aus, um zu erfahren,
wie Sie ihn aktualisieren können.  Siehe
@uref{https://guix.gnu.org/de/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
für mehr Informationen zu diesem Fehler.")
         (nl "Het vorige nieuwsbericht beschreef een beveiligingsprobleem in
@command{guix-daemon} dat kan leiden tot de escalatie van lokale bevoegdheden.
Het bericht stelde dat machines waarop de
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''}-optie van Linux is inschakeld niet kwetsbaar zijn.

Dit is niet volledig juist.  De optie maakt het uitbuiten van de fout
moeilijker maar niet onmogelijk.  Om onaangename verrassingen te voorkomen
is het voor iedereen aangeraden om @command{guix-daemon} op te waarderen.
Voer @command{info \"(guix) Upgrading Guix\"} uit voor meer informatie
daarover.  Lees
@uref{https://guix.gnu.org/en/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
voor meer informatie over het probleem.")))

 (entry (commit "ec7fb669945bfb47c5e1fdf7de3a5d07f7002ccf")
        (title
         (en "Risk of local privilege escalation @i{via} @command{guix-daemon}")
         (de "Risiko lokaler Rechteausweitung über @command{guix-daemon}")
         (fr "Risque d'élévation locale de privilèges @i{via} @command{guix-daemon}")
         (nl "Risico op escalatie van bevoegdheden via @command{guix-daemon}"))
        (body
         (en "A security vulnerability that can lead to local privilege
escalation has been found in @command{guix-daemon}.  It affects multi-user
setups in which @command{guix-daemon} runs locally.

It does @emph{not} affect multi-user setups where @command{guix-daemon} runs
on a separate machine and is accessed over the network, @i{via}
@env{GUIX_DAEMON_SOCKET}, as is customary on cluster setups.  Machines where
the Linux @uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''} feature is enabled, which is common, are also
unaffected---this is the case when the contents of
@file{/proc/sys/fs/protected_hardlinks} are @code{1}.

The attack consists in having an unprivileged user spawn a build process, for
instance with @command{guix build}, that makes its build directory
world-writable.  The user then creates a hardlink within the build directory
to a root-owned file from outside of the build directory, such as
@file{/etc/shadow}.  If the user passed the @option{--keep-failed} option and
the build eventually fails, the daemon changes ownership of the whole build
tree, including the hardlink, to the user.  At that point, the user has write
access to the target file.

You are advised to upgrade @command{guix-daemon}.  Run @command{info \"(guix)
Upgrading Guix\"}, for info on how to do that.  See
@uref{https://issues.guix.gnu.org/47229} for more information on this bug.")
         (de "Eine Sicherheitslücke, die zu einer lokalen Rechteausweitung
führen kann, wurde in @command{guix-daemon} gefunden.  Sie betrifft
Mehrbenutzersysteme, auf denen @command{guix-daemon} lokal läuft.

@emph{Nicht} betroffen sind Mehrbenutzersysteme, auf denen
@command{guix-daemon} auf einer separaten Maschine läuft und darauf über das
Netzwerk mittels @env{GUIX_DAEMON_SOCKET} zugegriffen wird, was auf
Rechen-Clustern üblich ist.  Auch Maschinen, auf denen Linux’
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
„Geschützte-Hardlinks“-Funktionalität} aktiviert ist@tie{}– was häufig der
Fall ist@tie{}–, sind nicht betroffen; sie ist aktiviert, wenn
@file{/proc/sys/fs/protected_hardlinks} den Inhalt @code{1} hat.

Der Angriff besteht darin, dass ein unprivilegierter Benutzer einen
Erstellungsprozess startet, etwa mit @command{guix build}, der allen
Schreibberechtigung auf sein Erstellungsverzeichnis erteilt.  In diesem
Erstellungsverzeichnis erzeugt der Benutzer nun eine harte Verknüpfung auf
eine Datei außerhalb des Erstellungsverzeichnisses, die dem
Administratornutzer root gehört, etwa @file{/etc/shadow}.  Wenn der Nutzer die
Befehlszeilenoption @option{--keep-failed} angegeben hat und die Erstellung
irgendwann fehlschlägt, trägt der Daemon als Besitzer des gesamten
Erstellungsverzeichnisses den Benutzer ein, Hardlink eingeschlossen.  Jetzt
hat der Benutzer Schreibzugriff auf die Zieldatei bekommen.

Wir empfehlen, dass Sie @command{guix-daemon} aktualisieren.  Führen Sie
@command{info \"(guix.de) Aktualisieren von Guix\"} aus, um zu erfahren, wie
Sie ihn aktualisieren können.  Siehe @uref{https://issues.guix.gnu.org/47229}
für mehr Informationen zu diesem Fehler.")
         (fr "Une faille de sécurité pouvant mener à une élévation locale de
privilèges a été trouvée dans @command{guix-daemon}.  Elle touche les
installations multi-utilisateur·ices dans lesquelles @command{guix-daemon}
tourne en local.

Elle @emph{n'affecte pas} les installations où @command{guix-daemon} tourne
sur une machine séparée et qu'on y accède à travers le réseau, @i{via}
@env{GUIX_DAEMON_SOCKET}, comme c'est typiquement le cas sur les grappes de
calcul (@i{clusters}).  Les machines où les
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt, ``liens
protégés''} de Linux sont activés, ce qui est courant, ne sont pas non plus
touchées ; cette fonctionnalité est activée si le contenu de
@file{/proc/sys/fs/protected_hardlinks} est @code{1}.

Pour mener cette attaque, un·e utilisateur·rice démarre un processus de
compilation, par exemple avec @command{guix build}, qui rend le répertoire de
compilation inscriptible pour tout le monde.  La personne créée ensuite un
lien dur (@i{hard link}) dans ce répertoire vers un fichier appartenant à
@code{root}, tel que @file{/etc/shadow}.  Si on a passé l'option
@option{--keep-failed} et que la compilation finit par échouer, le démon met
l'utilisateur·rice appelant·e comme propriétaire de l'ensemble du répertoire
de compilation, y compris le lien.  À ce stade, cette personne a accès en
écriture sur le fichier cible.

Nous conseillons de mettre à jour @command{guix-daemon}.  Lancer @command{info
\"(guix.fr) Mettre à niveau Guix\"} pour voir comment faire.  Voir
@uref{https://issues.guix.gnu.org/47229} pour plus d'informations sur cette
faille.")
                  (nl "In @command{guix-daemon} werd een beveiligingsprobleem
gevonden dat kan leiden tot de escalatie van lokale bevoegdheden.  Het
probleem doet zich voor bij installaties met meerdere gebruikers waarop een
lokale @command{guix-daemon} draait.

Het heeft @emph{geen} invloed op systemen met meerdere gebruikers waarbij de
@command{guix-daemon} op een afzonderlijke machine draait en via
@env{GUIX_DAEMON_SOCKET} over het netwerk wordt aangesproken, zoals
gebruikelijk bij computerclusters.  Ook machines waarop de
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''}-optie van Linux is inschakeld, wat vaak het geval is,
zijn niet kwetsbaar.

De aanval bestaat erin dat een gebruiker zonder privileges een bouwproces
opstart, bijvoorbeeld met @command{guix build}, dat zijn werkmap beschrijfbaar
maakt voor alle gebruikers.  Vervolgens maakt de gebruiker vanuit deze map een
harde link naar een bestand erbuiten met @code{root} als eigenaar, zoals
@file{/etc/shadow}.  Als de gebruiker de @option{--keep-failed}-optie opgaf
en de bouw faalt, maakt @command{guix-daemon} de gebruiker eigenaar van de
volledige inhoud van de werkmap, met inbegrip van de harde link.  Op dat
moment bezit de gebruiker schrijfrechten over het doelbestand.

Het is aangeraden om @command{guix-daemon} op te waarderen.  Voer
@command{info \"(guix) Upgrading Guix\"} uit voor meer informatie daarover.
Lees @uref{https://issues.guix.gnu.org/47229} voor meer informatie over het
probleem.")))

 (entry (commit "77c2f4e2068ebec3f384c826c5a99785125ff72c")
        (title
         (en "@code{qemu-binfmt-service-type} is usable for any container")
         (de "@code{qemu-binfmt-service-type} funktioniert mit jedem Container")
         (fr "@code{qemu-binfmt-service-type} fonctionne avec tous les conteneurs"))
        (body
         (en "The service now makes use of the statically built QEMU binaries
along with the fix binary (F) @code{binfmt_misc} flag, which allows the kernel
to fully pre-load it in memory.  QEMU can thus now be used with any container
without extra configuration.  The @code{guix-support?} field of the
@code{qemu-binfmt-configuration} record is removed, as it is no longer
necessary.")
         (de "Der Dienst benutzt jetzt statisch gebundene QEMU-Binärdateien
zusammen mit der Fix-Binary-Flag (F) von @code{binfmt_misc}.  Dadurch kann der
Kernel die QEMU-Binärdatei als Ganzes vorab in den Speicher laden.  Dann kann
sie auch ohne weitere Konfiguration in jeder Art von isolierter Umgebung
benutzt werden. Darum wurde das Feld @code{guix-support?} des
@code{qemu-binfmt-configuration}-Verbundsobjekts entfernt; es wird nicht mehr
gebraucht.")
         (fr "Le service utilise maintenant les binaire QEMU statiques avec
le drapeau « fixed » (F) de @code{binfmt_misc}, ce qui permet au noyau
de le charger entièrement en mémoire.  On peut donc maintenant utiliser QEMU
avec n'importe quel conteneur sans configuration supplémentaire.  Le champ
@code{guix-support?} de l'enregistrement @code{qemu-binfmt-configuration} a
été supprimé car il n'est pas nécessaire.")))

 (entry (commit "02e2e093e858e8a0ca7bd66c1f1f6fd0a1705edb")
        (title
         (en "New @command{guix import go} command")
         (de "Neuer Befehl @command{guix import go}")
         (fr "Nouvelle commande @command{guix import go}")
         (nl "Nieuwe @command{guix import go}-opdracht"))
        (body
         (en "The new @command{guix import go} command allows packagers to
generate a package definition or a template thereof given the name of a Go
package available through @url{https://proxy.golang.org}, like so:

@example
guix import go golang.org/x/sys
@end example

Run @command{info \"(guix) Invoking guix import\"} for more info.")
         (de "Mit dem neuen Befehl @command{guix import go} können
Paketautoren eine Paketdefinition oder eine Vorlage dafür anhand des Namens
eines auf @url{https://proxy.golang.org} verfügbaren Go-Pakets erzeugen, etwa
so:

@example
guix import go golang.org/x/sys
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix import\"} aus, um mehr
Informationen zu bekommen.")
         (fr "La nouvelle commande @command{guix import go} permet aux
empaqueteur·ice·s de générer une définition de paquet ou un modèle de
définition à partir du nom d'un paquet Go disponible via
@url{https://proxy.golang.org}, comme ceci :

@example
guix import go golang.org/x/sys
@end example

Lancez @command{info \"(guix.fr) Invoquer guix import\"} pour en savoir plus.")
                  (nl "Met de nieuwe @command{guix import go}-opdracht kunnen
pakketschrijvers een pakketdefinitie of -sjabloon aanmaken, op basis van de
naam van een Go-pakket te vinden op @url{https://proxy.golang.org}:

@example
guix import go golang.org/x/sys
@end example

Voer @command{info \"(guix) Invoking guix import\"} uit voor meer
informatie.")))

 (entry (commit "1b5b882120daf7d111aa351a919a90e818324347")
        (title
          (en "The @code{linux-libre} kernel is updated to 5.11.2")
          (de "Der Kernel @code{linux-libre} wird auf 5.11.2 aktualisiert")
          (fr "Le noyau @code{linux-libre} est mis à jour vers la 5.11.2")
          (nl "De @code{linux-libre}-kernel werd bijgewertk naar 5.11.2"))
        (body
          (en "The default @code{linux-libre} kernel is now based on the 5.11
stable kernel series, beginning with version 5.11.2.  Promiment features include
improved Wine performance, unprivileged Overlayfs mounts, support for Intel SGX,
support for new graphics hardware, and improved performance of the Btrfs
file system.")
          (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
jetzt auf der 5.11-„stable“-Versionsreihe, angefangen mit Version 5.11.2.  Zu
den markanten Neuerungen gehören bessere Wine-Unterstützung, Einbinden per
Overlayfs für Nutzer ohne erweiterte Rechte, Unterstützung für Intel SGX, für
neue Grafikhardware und bessere Leistung beim Btrfs-Dateisystem.")
          (fr "Le noyau @code{linux-libre} par défaut est maintenant basé sur la
lignée stable 5.11 du noyau, à commencer par la version 5.11.2.  Parmi les
fonctionnalités notables on trouve des performances améliorées pour Wine, le
montage Overlayfs non privilégié, la prise en charge d'Intel SGX, celle des
nouveaux périphériques graphiques et de meilleures performances du système de
fichiers Btrfs.")
          (nl "De standaard @code{linux-libre}-kernel is nu geëent op de
stabiele 5.11-reeks, te beginnen met versie 5.11.2.  Deze update biedt onder
andere verbeterde prestaties voor Wine en het Btfrs-bestandssysteem, laat
gewone gebruikers toe om met Overlayfs bestandssystemen te combineren, en
ondersteunt Intel SGX en nieuwe grafische apparatuur.")))

 (entry (commit "6e8cdf1d26092cb9654e179b04730fff7c15c94f")
        (title
         (en "The @command{guix system image} command can now operate on image records")
         (de "Der Befehl @command{guix system image} kann jetzt auch mit @code{image}-Verbundsobjekten umgehen")
         (fr "La commande @command{guix system image} peut désormais fonctionner sur des images"))
        (body
         (en "The @command{guix system image} command can now operate on
@code{image} records.  This means that the file parameter or the expression
passed to this command can return @code{image} or @code{operating-system}
records.

The @file{gnu/system/images} directory contains default images that can be
built by running @command{guix system image gnu/system/images/pine64.scm} for
instance.")
         (de "Sie können den Befehl @command{guix system image} jetzt auch auf
Verbundsobjekte vom Typ @code{image} anwenden.  Das heißt, wenn Sie eine Datei
oder einen Ausdruck als Parameter übergeben, darf dieser ein Verbundsobjekt
vom Typ @code{image} oder @code{operating-system} zurückliefern.

Im Verzeichnis @file{gnu/system/images} finden Sie vorkonfigurierte Abbilder
als @code{image}-Verbundsobjekte. Sie können zum Beispiel @command{guix system
image gnu/system/images/pine64.scm} ausführen, um das Abbild zu erstellen.")
         (fr "La commande @command{guix system image} peut désormais
fonctionner sur des images.  Cela signifie que le fichier ou l'expression
passé en paramètre de cette commande peuvent retourner une structure de type
@code{image} ou @code{operating-system}.

Le dossier @file{gnu/system/images} contient des images par défaut qui peuvent
être construites en lançant la commande @command{guix system image
gnu/system/images/pine64.scm} par exemple.")))

 (entry (commit "aa8de806252e3835d57fab351b02d13db762deac")
        (title
         (en "Risk of local privilege escalation @i{via} setuid programs")
         (de "Risiko lokaler Rechteausweitung bei setuid-Programmen")
         (fr "Risque de gain local de privilèges @i{via} les programmes setuid")
         (zh "存在通过 setuid 程序进行本地提权的风险"))
        (body
         (en "On Guix System, setuid programs were, until now, installed as
setuid-root @emph{and} setgid-root (in the @file{/run/setuid-programs}
directory).  However, most of these programs are meant to run as setuid-root,
but not setgid-root.  Thus, this setting posed a risk of local privilege
escalation.

This bug has been fixed and users are advised to upgrade their system, with a
command along the lines of:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Users of Guix on a ``foreign distro'' are unaffected.  See
@url{https://issues.guix.gnu.org/46395} for more information.")
         (de "Auf Guix System wurden setuid-Programme bisher mit setuid-root
@emph{und} setgid-root ausgestattet (im Verzeichnis
@file{/run/setuid-programs}).  Die meisten solchen Programme sind jedoch nur
dafür gedacht, mit setuid-root zu laufen, ohne setgid-root.  Durch diese
Einstellung war daher vielleicht eine lokale Rechteausweitung („local
privilege escalation“) möglich.

Dieser Fehler wurde behoben und Benutzern wird geraten, ihr System zu
aktualisieren, etwa mit diesem Befehl:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Benutzer von Guix auf einer „Fremddistribution“ sind @emph{nicht} betroffen.
Siehe @url{https://issues.guix.gnu.org/46395} für weitere Informationen.")
         (fr "Sur Guix System, les programmes setuid étaient jusqu'à présent
installés setuid-root @emph{et} setgid-root (dans le répertoire
@file{/run/setuid-programs}).  Ces programmes sont généralement conçus pour
être setuid-root, mais pas setgid-root, et cette situation posait donc un
risque de gain local de privilèges.

Ce problème est corrigé et vous êtes encouragé·e à mettre à jour votre
système, avec une commande de ce genre :

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Les usagers de Guix sur une distrib externe ne sont pas touché·es.  Plus
d'informations sont disponibles à @url{https://issues.guix.gnu.org/46395} (en
anglais).")
         (zh "到目前为止，Guix 系统上的 setuid 程序（位于 @file{/run/setuid-programs}）
同时具有 setuid-root @emph{和} setgid-root 权限。然而，此类程序大多被设计为在拥有
setuid 权限而非 setgid 权限时运行。因此，这样的设置可能会使系统受到本地提权攻击。

此漏洞已经被修复，同时建议用户使用下列命令升级他们的系统：

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

在 ``第三方宿主系统'' 上使用 Guix 的用户不受此漏洞影响，详情请参阅
@url{https://issues.guix.gnu.org/46395}。")))

 (entry (commit "aedbc5ff32a62f45aeed74c6833399a6cf2c22dc")
        (title
         (en "Create a manifest with @command{guix package --export-manifest}")
         (de "Manifest erzeugen mit @command{guix package --export-manifest}")
         (fr "Créer un manifeste avec @command{guix package --export-manifest}"))
        (body
         (en "The @command{guix package --export-manifest} command outputs a
@dfn{manifest} from your profile.  This manifest is a code snippet that can
then be passed to @command{guix package --manifest} (or any other command that
accepts the @option{--manifest} option) to deploy these packages.

The goal of this new @option{--export-manifest} option is to make it easier to
migrate from an ``imperative'' style where you repeatedly invoke @command{guix
install} and similar commands, to the declarative style where you write in a
manifest file the list of packages you want to have.

Similarly, the new @option{--export-channels} option outputs a @dfn{channel
specification} suitable for @command{guix pull --channels} from your profile.
This allows you to ``pin'' Guix to the revision that was used to build the
profile.

Run @command{info \"(guix) Invoking guix package\"} for more info.")
         (de "Mit dem Befehl @command{guix package --export-manifest} wird ein
@dfn{Manifest} aus Ihrem Profil erzeugt. Bei einem Manifest handelt es sich um
ein Stück Code, das Sie an @command{guix package --manifest} zum Einspielen
der Pakete aus dem Manifest übergeben können (oder an jeden anderen Befehl,
der die Befehlszeilenoption @option{--manifest} versteht).

Die Absicht hinter dieser neuen Befehlszeilenoption @option{--export-manifest}
ist, dass man leichter von einem „imperativen“ Stil, bei dem man wiederholt
@command{guix install} und ähnliche Befehle aufruft, zum deklarativen Stil
wechseln kann. Im deklarativen Stil tragen Sie die Liste der Pakete, die Sie
haben möchten, in eine Manifest-Datei ein.

Analog können Sie mit der neuen Befehlszeilenoption @option{--export-channels}
zu Ihrem Profil eine @dfn{Kanalspezifikation} erzeugen, die für @command{guix
pull --channels} geeignet ist.  Damit können Sie für Guix immer die Version
benutzen, mit der das Profil erstellt wurde.

Führen Sie für mehr Informationen @command{info \"(guix.de) Aufruf von guix
package\"} aus.")
         (fr "La commande @command{guix package --export-manifest} affiche un
@dfn{manifeste} pour le profil choisi.  Ce manifeste est un bout de code qu'on
peut passer à @command{guix package --manifest} (ou n'importe qu'elle commande
qui accepte l'option @option{--manifest}) pour déployer ces paquets.

L'objectif de cette nouvelle option @option{--export-manifest} est de
faciliter la migration du modèle ``impératif'', où on utilise @command{guix
install} et les commandes de ce genre, au modèle déclaratif où on écrit dans
un fichier la liste des paquets que l'on veut avoir.

De même, la nouvelle option @option{--export-channels} produit une
@dfn{spécification de canaux} pour @command{guix pull --channels} à partir du
profil.  Cela permet de ``figer'' Guix à la révision qui a été utilisée pour
produire le profil.

Voir @command{info \"(guix.fr) Invoquer guix package\"} pour plus
d'informations.")))

 (entry (commit "9ab817b2a4601b4a6755983590ed7d93ebdc8d09")
        (title (en "New @option{--with-latest} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-latest}")
               (fr "Nouvelle option de transformation @option{--with-latest}"))
        (body
         (en "The new @option{--with-latest} package transformation option
gets the latest release of a package, as would be identified by @command{guix
refresh}, and uses it instead of the currently-packaged version.  For example,
to install the latest release of GNOME Weather linked against the latest
version of libgweather, run:

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Mit der neuen Paketumwandlungsoption @option{--with-latest} wird
die neueste Veröffentlichung für ein Paket verwendet.  Diese wird wie bei
@command{guix refresh} bestimmt und anstelle der zurzeit im Paket festgelegten
Version verwendet.  Um zum Beispiel die neuste Veröffentlichung von GNOME
Weather gebunden an die neuste Version von libgweather zu installieren, führen
Sie dies aus:

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-latest} récupère la dernière version d'un logiciel telle
qu'elle serait trouvée par @command{guix refresh} et l'utilise à la place la
version actuellement fournie par le paquet.  Par exemple, pour installer la
dernière version de GNOME Weather, elle-même compilée avec la dernière version
de libgweather, on lancera :

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")))

 (entry (commit "a879e35116043d5daf3d9d175b697d10b9177fd5")
        (title (en "Substitutes can now be compressed with zstd")
               (de "Substitute können nun mit zstd komprimiert werden")
               (fr "Les substituts peuvent maintenant être compressés avec zstd"))
        (body
         (en "The @command{guix publish} command now supports substitute
compression with zstd and @command{guix-daemon} can now fetch and decompress
them.

The advantage of zstd over the other options is its high compression and
decompression throughput, with good compression ratios (not as good as lzip,
but slightly better than gzip).  Its high decompression throughput makes it a
good choice in situations where substitute downloads would otherwise be
CPU-bound, typically when having a high-speed connection to the substitute
server.  Run @command{info \"(guix) Invoking guix publish\"} for more info.

To be able to fetch zstd-compressed substitutes (if the substitute servers you
chose provide them), you need to upgrade your daemon.  Run @command{info
\"(guix) Upgrading Guix\"} to learn how to do it.")
         (de "Mit dem Befehl @command{guix publish} können Sie jetzt auch
Substitute mit zstd komprimieren und @command{guix-daemon} kann sie laden und
dekomprimieren.

zstd bietet gegenüber den anderen Optionen einen hohen Durchsatz bei
Kompression und Dekompression mit guten Kompressionsverhältnissen (nicht so
gut wie lzip, aber etwas besser als gzip).  Wegen des hohen Durchsatzes bei
der Dekompression ist zstd eine gute Wahl, wenn beim Herunterladen von
Substituten ansonsten der Engpass bei der Prozessorleistung läge, etwa weil
eine schnelle Netzwerkverbindung zum Substitutserver besteht.  Führen Sie für
mehr Informationen @command{info \"(guix.de) Aufruf von guix publish\"} aus.

Um zstd-komprimierte Substitute benutzen zu können (wenn der Substitutserver
sie anbietet), müssen Sie Ihren Daemon aktualisieren.  Führen Sie
@command{info \"(guix.de) Aktualisieren von Guix\"} aus, um zu erfahren, wie
Sie ihn aktualisieren.")
         (fr "La commande @command{guix publish} peut maintenant compresser
les substituts avec zstd et @command{guix-daemon} est capable de les récupérer
et de les décompresser.

L'avantage de zstd par rapport aux autres méthodes est son haut débit en
compression et décompression, avec un taux de compression correct (pas aussi
bon que lzip, mais légèrement meilleur que gzip).  Sa vitesse de décompression
en fait un bon choix dans les situations où le temps de téléchargement des
substituts se retrouve sinon limité par le temps de calcul comme c'est le cas
lorsqu'on bénéficie d'une connexion rapide au serveur de substitut.  Lancer
@command{info \"(guix.fr) Invoquer guix publish\"} pour plus d'informations.

Pour pouvoir télécharger des substituts compressés avec zstd (si les serveurs
de substituts choisis les fournissent), il faudra d'abord mettre à jour le
démon.  Lancer @command{info \"(guix.fr) Mettre à niveau Guix\"} pour voir
comment faire.")))

 (entry (commit "e38d90d497e19e00263fa28961c688a433154386")
        (title (en "New @option{--with-patch} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-patch}")
               (fr "Nouvelle option de transformation @option{--with-patch}"))
        (body
         (en "The new @option{--with-patch} package transformation option
applies patches to the specified packages before building them.  The example
below builds the GNU Core Utilities against a patched C library (glibc):

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-patch} wendet
Patches auf die angegebenen Pakete an, bevor sie erstellt werden. Das folgende
Beispiel lässt die GNU Core Utilities mit einer gepatchten
C-Bibliothek (glibc) erstellen:

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-patch} applique des modifications (@i{patches}) aux paquets
spécifiés avant de les compiler.  L'exemple suivant compile les utilitaires de
base GNU avec une bibliothèque C (glibc) modifiée :

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")))

 (entry (commit "79f9dee3c4c0e6d21066f142116a537207ae7ba4")
        (title (en "Local substitute servers discovery is now supported")
               (de "Substitutserver können jetzt im lokalen Netz erkannt werden")
               (es "Los servidores de sustituciones se pueden descubrir localmente")
               (fr "La découverte des serveurs de substituts locaux est désormais supportée"))
        (body
         (en "The @command{guix-daemon} can now discover local substitute
servers when the @option{--discover} option is passed.  Only the substitute
servers started with the @option{--advertise} option will be discovered.  The
network discovery is based on mDNS and DNS-SD protocols, using Guile-Avahi
library for now.")
         (de "Mit dem @command{guix-daemon} können jetzt lokal laufende
Substitutserver erkannt werden, wenn die Befehlszeilenoption
@option{--discover} übergeben wurde.  Nur solche Substitutserver werden
gefunden, die mit der Befehlszeilenoption @option{--advertise} gestartet
wurden.  Die Ermittlung im Netzwerk verfügbarer Substitutserver baut auf den
Protokollen mDNS und DNS-SD auf.  Derzeit wird dazu die Bibliothek Guile-Avahi
benutzt.")
         (es "El daemon @command{guix-daemon} ahora puede descubrir servidores
de sustituciones locales cuando se le proporciona la opción
@option{--discover}.  Únicamente se descubrirán los servidores de
sustituciones que se hayan arrancado con la opción @option{--advertise}. La
búsqueda en la red se basa en los protocolos mDNS y DNS-SD, actualmente
mediante el uso de la biblioteca Guile-Avahi.")
         (fr "Le @command{guix-daemon} peut désormais découvrir les serveurs
de substituts locaux lorsque l'option @option{--discover} est passée.  Seuls
les serveurs de substituts démarrés avec l'option @option{--advertise} seront
découverts.  La découverte réseau utilise les protocoles mDNS et DNS-SD, pour
l'instant grâce à la librairie Guile-Avahi.")))

 (entry (commit "a9a2fdaabcc78e7a54d9a6bcfa4ee3de308e9a90")
        (title (en "Logical Volume Manager (LVM) now supported on Guix System")
               (de "Logical Volume Manager (LVM) wird jetzt auf Guix System unterstützt")
               (es "El sistema Guix ahora implementa también volúmenes lógicos LVM")
               (fr "Le gestionnaire de volumes logiques (LVM) est maintenant pris en charge par le système Guix"))
        (body
         (en "On Guix System, the new @code{lvm-device-mapping} variable
allows you to declare ``mapped devices'' for LVM, the Linux Logical Volume
Manager.  For example, LVM logical volumes ``alpha'' and ``beta'' from volume
group ``vg0'' can be declared as follows:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

See @command{info \"(guix) Mapped Devices\"} for more information.")
         (de "Auf Guix System erlaubt Ihnen die neue Variable
@code{lvm-device-mapping}, „zugeordnete Geräte“ (Mapped Devices) für LVM, den
Linux Logical Volume Manager, zu deklarieren. Zum Beispiel können logische
Datenträger von LVM namens „alpha“ und „beta“ aus der
Datenträgergruppe (Volume Group) „vg0“ wie folgt deklariert werden:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Siehe @command{info \"(guix.de) Zugeordnete Geräte\"} für nähere Informationen.")
         (es "En el sistema Guix, la nueva variable @code{lvm-device-mapping}
le permite declarar «dispositivos traducidos» para LVM, el gestor de volúmenes
lógicos de Linux. A continuación se muestra un ejemplo con la declaración de
los volúmenes lógicos «alfa» y «beta» del grupo de volúmenes «vg0»:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alfa\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Véase @command{info \"(guix.es) Dispositivos traducidos\"} para obtener más
información.")
         (fr "Sur le système Guix, la nouvelle variable @code{lvm-device-mapping}
vous permet de déclarer des « périphériques mappés » pour LVM, le gestionnaire
de volumes logiques.  Par exemple, vous pouvez déclarer les volumes logiques
« alpha » et « beta » du groupe « vg0 » comme ceci :

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Voir @command{info \"(guix.fr) Périphériques mappés\"} pour en savoir plus.")))

 (entry (commit "3b6e4e5fd05e72b8a32ff1a2d5e21464260e21e6")
        (title (en "List of substitute keys is now declarative on Guix System")
               (de "Liste der Substitutschlüssel auf Guix System ist jetzt deklarativ")
               (es "Claves para sustituciones del sistema Guix en formato declarativo")
               (fr "Liste des clefs de substituts désormais déclarative sur Guix System"))
        (body
         (en "The list of authorized substitute keys, available in
@file{/etc/guix/acl}, is now built by default in a purely declarative fashion
on Guix System based on the @code{authorized-keys} field of the configuration
of @code{guix-service-type}.  This means that manual changes to
@file{/etc/guix/acl} are now @emph{discarded} upon reconfiguration or
reboot (a backup is made as @file{/etc/guix/acl.bak} in that case).

We recommend updating your operating system configuration to explicitly list
all the authorized substitute keys.  See @command{info \"(guix) Base
Services\"}, for more info about @code{guix-configuration} and
@code{authorized-keys}.

Alternatively, you can set the @code{authorize-key?} field of
@code{guix-configuration} to @code{#f} to restore previous behavior.")
         (de "Die Liste von autorisierten Substitutschlüsseln, die in
@file{/etc/guix/acl} steht, wird auf Guix System nach Vorgabe jetzt auf rein
deklarative Weise erstellt, je nach Inhalt des @code{authorized-keys}-Feldes
der Konfiguration des @code{guix-service-type}. Das hat zur Folge, dass
manuelle Änderungen an @file{/etc/guix/acl} von jetzt an nach jedem
Rekonfigurieren oder Neustarten @emph{verworfen} werden (in diesem Fall wird
eine Sicherheitskopie namens @file{/etc/guix/acl.bak} angelegt).

Wir empfehlen, dass Sie Ihre Betriebssystemkonfiguration aktualisieren, damit
dort alle autorisierten Substitutschlüssel ausdrücklich aufgeführt
werden. Siehe @command{info \"(guix.de) Basisdienste\"} für mehr Informationen
zur @code{guix-configuration} und @code{authorized-keys}.

Alternativ können Sie das @code{authorize-key?}-Feld der
@code{guix-configuration} auf @code{#f} setzen, um zum alten Verhalten
zurückzugehen.")
         (es "El listado de claves autorizadas para la obtención de
sustituciones, disponible en @file{/etc/guix/acl}, ahora se genera de manera
predeterminada en el sistema Guix de forma completamente declarativa en base
al campo @code{authorized-keys} del la configuración para el servicio
@code{guix-service-type}. Esto significa que los cambios que se hayan
realizado de manera manual en @file{/etc/guix/acl} @emph{se descartan} tras
una reconfiguración del sistema o tras un reinicio (se realiza una copia de
seguridad en la ruta @file{/etc/guix/acl.bak} en este caso).

Le recomendamos que actualice su configuración del sistema operativo para que
enumere explícitamente todas las claves que desea autorizar para la obtención
de sustituciones.  Véase @command{info \"(guix.es) Servicios base\"}, para
obtener más información sobre @code{guix-configuration} y
@code{authorized-keys}.

También puede proporcionar el valor @code{#f} en el campo
@code{authorize-key?} de @code{guix-configuration} para volver al
comportamiento que se obtenía con versiones previas.")
         (fr "La liste des clefs de substituts autorisées, stockée dans
@file{/guix/guix/acl}, est dorénavant construite par défaut de manière
déclarative sur Guix System, en se basant sur le champs @code{authorized-keys}
de la configuration de @code{guix-service-type}.  Cela signifie que les
modifications apportées manuellement à @file{/etc/guix/acl} seront désormais
@emph{perdues} lors d'une reconfiguration ou d'un redémarrage (dans ce cas une
sauvegarde est faite dans @file{/etc/guix/acl.bak}).

Nous recommandons de mettre à jour sa configuration de système d'exploitation
pour y lister explicitement les clefs autorisées.  Lancez @command{info
\"(guix.fr) Services de base\"} pour plus d'informations sur
@code{guix-configuration} et @code{authorized-keys}.

Il est également possible de mettre le champs @code{authorize-key?} de
@code{guix-configuration} à @code{#f} pour restaurer le comportement qui
prévalait jusqu'à maintenant.")))

 (entry (commit "6aeda81602555fbeac0c0a209e74f5262093b513")
        (title (en "New @option{--with-debug-info} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-debug-info}")
               (es "Nueva opción de transformación @option{--with-debug-info}")
               (fr "Nouvelle option de transformation @option{--with-debug-info}"))
        (body
         (en "The new @option{--with-debug-info} option builds a variant of a
package that includes debug info and grafts it onto the application you want
to debug.  Thus, only the package for which you want debug info needs to be
recompiled.  This is useful for packages that do not already have a
@code{debug} output.

For example, here is how you would obtain debug info for the @code{glib}
library so you can inspect it while debugging Inkscape:

@example
guix build --with-debug-info=glib inkscape
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-debug-info} lässt
eine Variante eines Pakets erstellen, die auch Informationen zur Fehlersuche
enthält. Damit wird die Anwendung veredelt, wo Sie Fehler nachvollziehen
möchten. Somit muss nur das Paket, für das Sie die Informationen brauchen, neu
kompiliert werden. Das ist hilfreich bei Paketen, die noch nicht über eine
@code{debug}-Ausgabe verfügen.

Zum Beispiel würden Sie so Informationen zur Fehlersuche für die
@code{glib}-Bibliothek bekommen, um sie inspizieren zu können, wenn Sie Fehler
in Inkscape nachvollziehen möchten:

@example
guix build --with-debug-info=glib inkscape
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (es "La nueva opción @option{--with-debug-info} construye una
variante del paquete que incluye la información de depuración y la injerta
en la aplicación que desee depurar.  Por tanto, únicamente el paquete del
que desee información de depuración debe construirse de nuevo.  Es útil
para paquetes que no tienen ya una salida @code{debug}.

El siguiente ejemplo muestra como obtener información de depuración
para la biblioteca @code{glib} de modo que pueda inspeccionarla mientras
depura Inkscape:

@example
guix build --with-debug-info=glib inkscape
@end example

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-debug-info} compile une variante d'un paquet avec les
informations de déboguage et la greffe sur l'application que l'on veut
déboguer.  Ainsi seul le paquet pour lequel on demande des informations de
déboguage a besoin d'être recompilé.  C'est utile pour les paquets n'ayant pas
déjà un résultat @code{debug}.

Voici par exemple comment obtenir des informations de déboguage pour la
bibliothèque @code{glib} de manière à pouvoir l'inspecter quand on débuggue
Inkscape :

@example
guix build --with-debug-info=glib inkscape
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")))

 (entry (commit "abd7a474615353149a44f4504f0b4b248dcc0716")
        (title (en "New @option{--with-c-toolchain} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-c-toolchain}")
               (es "Nueva opción de transformación @option{--with-c-toolchain}")
               (fr "Nouvelle option de transformation @option{--with-c-toolchain}"))
        (body
         (en "The new @option{--with-c-toolchain} package transformation
options provides an easy way for developers to rebuild their favorite packages
with the C/C++ tool chain of their choice instead of the default one.

For example, the following command rebuilds the @code{fftw} and @code{fftwf}
packages as well as every package that depends on them, up to and including
@code{octave-cli}, using GCC version 10 (currently GCC 7.5 is used by
default):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-c-toolchain}
bietet Entwicklern die Möglichkeit, leicht ihre Lieblingspakete mit der
selbstgewählten Toolchain für C/C++ anstelle der vorgegebenen neu zu
erstellen.

Zum Beispiel werden mit folgendem Befehl die Pakete @code{fftw} und
@code{fftwf} sowie alle davon abhängigen Pakete bis einschließlich
@code{octave-cli} mit Version 10 der GCC erstellt (vorgegeben wäre zurzeit,
GCC 7.5 zu benutzen):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (es "La nueva opción de transformación de paquetes
@option{--with-c-toolchain} proporciona a las desarrolladoras una manera
fácil de reconstruir sus paquetes favoritos con la cadena de herramientas
de compilación de C/C++ que elijan en vez de la predeterminada.

Por ejemplo, la siguiente orden reconstruye los paquetes @code{fftw} y
@code{fftwf} así como todos los paquetes que dependen de ellos hasta
@code{octave-cli}, usando la versión 10 de GCC (el compilador
predeterminado en estos momentos es GCC 7.5):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-c-toolchain} permet aux développeur·euses de recompiler leurs
paquets préférés avec la chaîne d'outils C/C++ de leur choix à la place de
celle par défaut.

Par exemple, la commande ci-dessous recompile @code{fftw}, @code{fftwf} et
tous les paquets qui en dépendent, jusqu'à @code{octave-cli} inclus, avec GCC
10 (actuellement c'est GCC 7.5 qui est utilisé par défaut):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")))

 (entry (commit "8e1907a72430aa989125b053573ef0897c480697")
        (title (en "Package transformation options now recorded in profiles")
               (es "Las opciones de transformación de paquetes ahora se
quedan registradas en el perfil")
               (de "Paketumwandlungsoptionen werden nun in Profilen gesichert")
               (fr "Options de transformation sauvegardées dans les profils"))
        (body
         (en "When installing packages in a profile, package transformation
options such as @option{--with-input} are now recorded in the profile.  When
you eventually run @command{guix upgrade}, those transformations will be
automatically applied to the upgraded packages.

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (es "Si durante la instalación de paquetes en un perfil se utilizaron
opciones de transformación de paquetes, como por ejemplo
@option{--with-input}, éstas se registran en el perfil. Cuando vuelva a
ejecutar @command{guix upgrade}, dichas transformaciones se aplicarán
automáticamente a los paquetes actualizados.

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (de "Wenn Sie ein Paket in ein Profil installieren, werden nun
Paketumwandlungsoptionen wie @option{--with-input} im Profil gespeichert.
Sobald Sie später @command{guix upgrade} ausführen, werden dieselben
Umwandlungen automatisch auf die aktualisierten Pakete angewandt.

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "Lorsqu'on installe des paquets dans un profil, les options de
transformation telles que @option{--with-input} sont désormais enregistrées
dans le profil.  Quand on le met plus tard à jour avec @command{guix upgrade},
ces transformations sont automatiquement appliquées aux nouveaux paquets.

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")))

 (entry (commit "313f492657f1d0863c641fa5ee7f5b7028e27c94")
        (title (en "New @option{--image-type} option for @command{guix system disk-image}.")
               (es "Nueva opción @option{--image-type} para @command{guix system disk-image}.")
               (de "Neue Option @option{--image-type} für @command{guix system disk-image}.")
               (fr "Nouvelle option @option{--image-type} pour @command{guix system disk-image}."))
        (body
         (en "The @option{--file-system-type} option for @command{guix system
disk-image} command has been replaced by the new @option{--image-type} option.
By default, @code{raw} disk images are produced, but @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} and @code{iso9660} image types
are also available.

The @option{--list-image-types} option lists all the available image types.")
         (es "La opción @option{--file-system-type} de @command{guix system
disk-image} se ha sustituido por la nueva opción @option{--image-type}.  De
manera predeterminada se producen imágenes en formato crudo (@code{raw}) pero
también están disponibles los tipos de imagen @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} y @code{iso9660}.

La opción @option{--list-image-types} muestra una lista con todos los tipos
de imagen disponibles.")
         (de "Anstelle der Befehlszeilenoption @option{--file-system-type} für
@command{guix system disk-image} gibt es nun die neue Option
@option{--image-type}.  In der Vorgabeeinstellung @code{raw} werden rohe
Disk-Images erzeugt, aber es können auch die Abbildtypen @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} und @code{iso9660} ausgewählt
werden.

Mit der Option @option{--list-image-types} werden alle verfügbaren Abbildtypen
aufgelistet.")
         (fr "L'option @option{--file-system-type} pour la commande
@command{guix system disk-image} a été remplacée par la nouvelle option
@option{--image-type}. Par défaut, l'option @code{raw}, produisant des images
disque brutes est sélectionnée. Les options @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} et @code{iso9660} sont également
disponibles.

La nouvelle option @option{--list-image-types} énumère les types d'image
disponibles.")))

 (entry (commit "8819551c8d2a12cd4e84e09b51e434d05a012c9d")
        (title (en "Package transformations now apply to implicit inputs")
               (es "Las transformaciones de paquetes ahora afectan también
a las dependencias implícitas")
               (de "Paketumwandlungen betreffen jetzt auch implizite Eingaben")
               (fr "Les transformations de paquets s'appliquent aux
dépendances implicites"))
        (body
         (en "Package transformation options such as @option{--with-branch},
@option{--with-input}, and so on now apply to implicit inputs---previously
only a package's explicit inputs would be affected.  This allows for things
such as replacing the Python dependency of a package that uses
@code{python-build-system}:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Another example is grafting a different version of the GNU C
Library (@code{glibc} is an implicit input of almost all the packages and is
``deep down'' in the dependency graph):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more
info.")
         (es "Las opciones de transformación de paquetes como
@option{--with-branch}, @option{--with-input}, etcétera, ahora también
influyen en las entradas implícitas---antes únicamente las entradas explícitas
del paquete se veían afectadas. Esto permite, por ejemplo, sustituir la
dependencia en python de un paquete que use @code{python-build-system}:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Otro ejemplo podría ser el injerto de una versión diferente de la biblioteca
de C de GNU (@code{glibc} es una entrada implícita de casi todos los paquetes
y ``muy abajo'' en el grafo de dependencias):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (de "Paketumwandlungsoptionen wie @option{--with-branch},
@option{--with-input} und so weiter betreffen nun auch implizite Eingaben —
zuvor haben sie sich nur auf die expliziten Eingaben eines Pakets
ausgewirkt. Dadurch kann jetzt zum Beispiel die Python-Abhängigkeit eines
Pakets, welches @code{python-build-system} benutzt, ersetzt werden:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Ein weiteres Beispiel ist, mit einer anderen Version der GNU-C-Bibliothek zu
veredeln (@code{glibc} ist eine implizite Eingabe fast aller Pakete und steckt
„ganz tief“ im Abhängigkeitsgraphen):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "Les options de transformation de paquets telles que
@option{--with-branch} et @option{--with-input} s'appliquent désormais aux
dépendances implicites — jusque là seules les dépendances explicites des
paquets étaient prises en compte.  Cela permet certaines choses telles que
remplacer la dépendance sur Python d'un paquet utilisant
@code{python-build-system} :

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Un autre exemple est la possibilité de greffer une version différente de la
bibliothèque C GNU (la @code{glibc} est une dépendance implicite de tous les
paquets et se trouve « tout en bas » du graphe de dépendance) :

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")))

 (entry (commit "f458cfbcc54ed87b1a87dd9e150ea276f17eab74")
        (title (en "New @option{--without-tests} transformation option")
               (es "Nueva opción de transformación @option{--without-tests}")
               (de "Neue Paketumwandlungsoption @option{--without-tests}")
               (fr "Nouvelle option de transformation @option{--without-tests}"))
        (body
         (en "The new @option{--without-tests} package transformation option
instructs Guix to skip the test suite of a given package.  In the example
below, @code{guile-gcrypt} is built using a variant of @code{automake} itself
built without running its (lengthy) test suite:

@example
guix build guile-gcrypt --without-tests=automake
@end example

This is primarily useful as a way to speed up development cycles, or to work
around flaky test suites---skipping tests can hide real issues, so use with
care.  Run @command{info \"(guix) Package Transformation Options\"} for more
info.")
         (es "La nueva opción de transformación de paquetes
@option{--without-tests} indica a Guix que omita la batería de pruebas del
paquete proporcionado. En el siguiente ejemplo @code{guile-gcrypt} se
construye usando una variación de @code{automake}, la cual se ha construido
sin ejecutar su (larga) batería de pruebas:

@example
guix build guile-gcrypt --without-tests=automake
@end example

Esto es principalmente útil como una forma de acelerar ciclos de desarrollo o
de omitir temporalmente baterías de pruebas problemáticas---omitir las pruebas
puede ocultar problemas reales, por lo que debe usarse con precaución.
Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (de "Mit der neuen Paketumwandlungsoption @option{--without-tests}
wird Guix angewiesen, den Testkatalog des angegebenen Pakets zu
überspringen. Im folgenden Beispiel wird @code{guile-gcrypt} mit einer
Variante von @code{automake} erstellt, die wiederum ohne Durchlauf ihres (viel
Zeit in Anspruch nehmenden) Testkatalogs erstellt wird:

@example
guix build guile-gcrypt --without-tests=automake
@end example

Der hauptsächliche Nutzen liegt in der Beschleunigung von Entwicklungszyklen
oder im Umgehen unzuverlässiger Testkataloge. Allerdings kann das Überspringen
dazu führen, dass echte Probleme verborgen bleiben. Setzen Sie es mit Bedacht
ein. Führen Sie @command{info \"(guix.de) Paketumwandlungsoptionen\"} aus, um
mehr Informationen zu erhalten.")
         (fr "La nouvelle option de transformation de paquets
@option{--without-tests} demande à Guix de sauter la suite de tests d'un
paquet.  Dans l'exemple ci-dessous, @code{guile-gcrypt} est construit en
utilisant une variante de @code{automake} construite sans lancer sa suite de
tests :

@example
guix build guile-gcrypt --without-tests=automake
@end example

Cette option est surtout intéressante pour raccourcir le cycle de
développement ou pour contourner une suite de tests qui n'est pas
fiable — sauter les tests peut cacher des vrais problèmes, à utiliser avec
précaution donc.  Voir @command{info \"(guix.fr) Options de transformation de
paquets\"} pour plus de détails.")))

 (entry (commit "a98712785e0b042a290420fd74e5a4a5da4fc68f")
        (title (en "New @command{guix git authenticate} command")
               (es "Nueva orden @command{guix git authenticate}")
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
         (es "La nueva orden @command{guix git authenticate} comprueba la
validez de un repositorio git verificando las firmas de las revisiones y
comprobando que todas las firmas están autorizadas, exactamente igual que
@command{guix pull}.

Esta orden es principalmente útil para desarrolladoras de canales. Permite
asegurar, antes de subir nada al repositorio remoto, que el canal contiene
únicamente revisiones firmadas por claves autorizadas. No obstante esta orden
es útil siempre que use git y quiera que otras personas puedan verificar el
código obtenido de su repositorio.

Ejecute @command{info \"(guix.es) Invocación de guix git authenticate\"}
para obtener más información y vea detalles sobre estos mecanismos en
 @uref{https://guix.gnu.org/blog/2020/securing-updates/}.")
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
               (es "@command{guix pull} verifica los canales")
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
         (es "Las ordenes @command{guix pull} y @command{guix time-machine}
ahora verifican el código fuente que obtienen, a menos que se proporcione la
opción @option{--disable-authentication}. Lo que esto significa es que Guix se
asegura de que cada revisión que recibe está firmada criptográficamente por
una desarrolladora autorizada. Esto le protege de intentos de modificación del
repositorio de Guix y de entregas de código con malas intenciones sobre las
usuarias.

Esta característica está limitada actualmente al canal @code{guix} pero pronto
estará disponible para autoras de canales independientes.")
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
               (es "@command{guix repl} puede ejecutar guiones de Guile")
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
         (es "La orden @command{guix repl} ahora se puede usar para
ejecutar guiones de Guile. En comparación con únicamente la ejecución
de la orden @command{guile}, @command{guix repl} garantiza que todos
los módulos de Guix y sus dependencias están disponibles en la ruta
 de búsqueda. Los guiones se ejecutan de este modo:

@example
guix repl -- mi-guion.scm --opcion1 --opcion2=param-op2 param1 param2
@end example

Ejecute @command{info \"(guix.es) Invocación de guix repl\"} para obtener
más información.")
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
               (es "Implementado el arranque desde un subvolumen de Btrfs")
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
         (es "El fichero de configuración de GRUB producido por la
declaración de sistema operativo ahora tiene en cuenta el uso de
subvolúmenes de Btrfs en la partición que contiene @file{/gnu/store}.
Ejecute la orden @command{info \"(guix.es) Sistema de ficheros Btrfs\"}
 para obtener más información y ejemplos.")
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
               (es "@command{guix pack -RR} introduce un nuevo motor
de ejecución")
               (de "@command{guix pack -RR} führt neuen Ausführungstreiber
ein")
               (fr "@command{guix pack -RR} introduit un nouveau moteur d'exécution"))
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
         (es "La orden @command{guix pack -RR} le permite crear un
archivador tar que contiene @dfn{binarios reposicionables}. Hasta ahora
dichos binarios dependían o bien de los ``espacios de nombres de usuarias
sin privilegios'' de Linux o en PRoot, cuando estos no estaban
implementados. No obstante, PRoot introduce una sobrecarga significativa
en algunos escenarios de trabajo.

Para estos casos @command{guix pack -RR} introduce una tercera opción
basada en una extensión al enlazador de tiempo de ejecución de GNU (ld.so)
y en Fakechroot, lo que conlleva muy poca sobrecarga. Puede seleccionar
la opción más rápida cuando ejecute un binario reposicionable de esta
manera:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Ejecute @command{info \"(guix.es) Invocación de guix pack\"} para
obtener más información.")
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
mehr wissen wollen.")
         (fr "La commande @command{guix pack -RR} vous permet de créer une
archive tar contenant des @dfn{binaires repositionnables}.  Jusqu'ici, ils
s'appuyaient sur les « espaces de noms non privilégiés » de Linux ou sur
PRoot, quand les espaces de noms non privilégiés n'étaient pas disponibles.
Cependant, PRoot ralenti significativement certains profils d'exécution.

Pour éviter cela, @command{guix pack -RR} introduit une troisième possibilité
basée sur une extension de l'éditeur des liens à l'exécution de GNU (ld.so) et
sur Fakechroot, qui ralenti très peu l'exécution.  Vous pouvez choisir l'option
la plus rapide à l'exécution d'un binaire relocalisable de cette manière :

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Lancez @command{info \"(guix.fr) Invoquer guix pack\"} pour en savoir plus.")))

 (entry (commit "88a96c568c47c97d05d883ada5afbc4e1200b10f")
        (title (en "New @option{--path} option for @command{guix graph}")
               (es "Nueva opción @option{--path} para @command{guix graph}")
               (de "Neue Option @option{--path} für @command{guix graph}")
               (fr "Nouvelle option @option{--path} pour @command{guix graph}"))
        (body
         (en "The @command{guix graph} command has a new @option{--path}
option that instructs it to display the shortest path between two packages,
derivations, or store items.  For example, the command below displays the
shortest path from the @code{libreoffice} package to @code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Run @code{info \"(guix) Invoking guix graph\"} for more information.")
         (es "La orden @command{guix graph} tiene una nueva opción
@option{--path} que le indica que debe mostrar la ruta más corta entre dos
paquetes, derivaciones o elementos del almacén. Por ejemplo, la siguiente
orden muestra la ruta más corta desde el paquete @code{libreoffice} hasta
@code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Ejecute @code{info \"(guix.es) Invocación de guix graph\"} para obtener más
información.")
         (de "Der Befehl @command{guix graph} verfügt über eine neue
Befehlszeilenoption @option{--path}, die ihn den kürzesten Pfad zwischen zwei
Paketen, Ableitungen oder Store-Objekten ausgeben lässt.  Zum Beispiel zeigt
folgender Befehl den kürzesten Pfad vom Paket @code{libreoffice} zu
@code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Führen Sie @code{info \"(guix.de) Aufruf von guix graph\"} aus, um mehr zu
erfahren.")
         (fr "La commande @command{guix graph} a une nouvelle option
@option{--path} qui lui dit d'afficer le plus court chemin entre deux
paquets, dérivations ou éléments du dépôt.  Par exemple, la commande ci-dessous
affiche le plus court chemin entre le paquet @code{libreoffice} et
@code{libunistring} :

@example
guix graph --path libreoffice libunistring
@end example

Lancez @code{info \"(guix.fr) Invoquer guix graph\"} pour en savoir plus.")))

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
               (de "Bootstrapping jetzt mit noch kleinerem Seed")
               (fr "Le bootstrap binaire est encore plus réduit"))
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
den Vortrag auf @uref{https://fosdem.org/2020/schedule/event/gnumes/} an.")
         (fr "Le graphe des paquets sur x86_64 et i686 prend maintenant racine
dans un @dfn{ensemble de graines binaires} plus réduit.  L'ensemble initial
des binaires à partir desquels les paquets sont désormais construit pèse
environ 60 Mo, un quart de ce qu'il était.  Lancez
@code{info \"(guix.fr) Bootstrapping\"} pour en savoir plus, ou regardez
la présentation sur @uref{https://fosdem.org/2020/schedule/event/gnumes/}.")))

 (entry (commit "0468455e7d279c89ea3ad1b51935efb2b785ec47")
        (title (en "Rottlog service added to @code{%base-services}")
               (de "Rottlog-Dienst ist nun Teil der @code{%base-services}")
               (fr "Le service rottlog a été ajouté à @code{%base-services}"))
        (body (en "An instance of @code{rottlog-service-type}, the system
service responsible for log rotation, has been added to @code{%base-services}.
If your operating system configuration for Guix System is explicitly adding
@code{rottlog-service-type} to the services, you should now remove it.  See
the ``Log Rotation'' section of the manual for more information.")
              (de "Eine Instanz des @code{rottlog-service-type} für
Log-Rotation wurde zu den @code{%base-services} hinzugefügt.  Wenn der
Systemdienst bereits in Ihrer Konfiguration für Guix System ausdrücklich
genannt wurde, sollten Sie ihn jetzt daraus entfernen.  Siehe den Abschnitt
„Log-Rotation“ im Handbuch für weitere Informationen.")
              (fr "Une instance de @code{rottlog-service-type}, le service
système responsable de la rotation des journaux, a été ajoutée à
@code{%base-services}.  Si votre configuration de système d'exploitation Guix
System ajoute @code{rottlog-service-type} explicitement, vous devriez maintenant
le supprimer.  Voir la section « Rotation des journaux » dans le manuel
pour en savoir plus.")))

 (entry (commit "b6bee63bed4f013064c0d902e7c8b83ed7514ade")
        (title (en "@code{guile} package now refers to version 3.0")
               (de "Das @code{guile}-Paket bezeichnet jetzt Version 3.0")
               (fr "Le paquet @code{guile} se réfère maintenant à la version 3.0"))
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
laufen auch auf 3.0.")
              (fr "Le paquet @code{guile} a été mis à jour vers la version 3.0
(au lieu de la 2.2).  Les paquets @code{guile3.0-} ont été renommés en leur
nom d'origine et les variantes @code{guile2.2-} de ces paquets ont été définis.
En plus, les dérivation sont maintenant construites avec Guile 3.0, et les
services systèmes utilisent aussi la 3.0.")))

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
               (de "@command{guix graph} unterstützt nun Paketumwandlungen")
               (fr "@command{guix graph} prend maintenant en charge les
transformations de paquets"))
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
de dependencias @option{--with-input}.")
         (fr "La commande @command{guix graph} prend maintenant en charge les
transformations de paquets communes (voir @command{info \"(guix.fr) Options de
transformation de paquets\"}).  C'est particulièrement utile pour voir l'effet
de l'option @option{--with-input} qui réécrit de graphe de dépendance.")))

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
