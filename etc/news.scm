;; GNU Guix news, for use by 'guix pull'.
;;
;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "81c580c8664bfeeb767e2c47ea343004e88223c7")
        (title (en "Insecure @file{/var/guix/profiles/per-user} permissions")
               (nl "Onveilige @file{/var/guix/profiles/per-user}-rechten"))
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
         (nl "Het standaard gebruikersprofiel, @file{~/.guix-profile}, verwijst
naar @file{/var/guix/profiles/per-user/$USER}.  Tot op heden kon om het even wie
in @file{/var/guix/profiles/per-user} schrijven, wat het @command{guix}-commando
toestond de @code{$USER} submap aan te maken.

Op systemen met meerdere gebuikers kon hierdoor een kwaadaardige gebruiker een
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
uit voor verdere uitleg." )))
 (entry (commit "cdd3bcf03883d129581a79e6d6611b2afd3b277b")
        (title (en "New reduced binary seed bootstrap")
               (de "Neues Bootstrapping mit kleinerem Seed")
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
         (nl "Het netwerk van pakketten voor x86_64 en i686 is nu geworteld in
een @dfn{verkleinde verzameling van binaire kiemen}.  Die beginverzameling
van binaire bestanden waaruit pakketten gebouwd worden is nu zo'n 130 MiB
groot; nog maar half zo groot als voorheen.  Voer @code{info \"(guix)
Bootstrapping\"} uit voor meer details, of bekijk de presentatie op
@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")))

 (entry (commit "dcc90d15581189dbc30e201db2b807273d6484f0")
        (title (en "New channel news mechanism")
               (de "Neuer Mechanismus, um Neuigkeiten über Kanäle anzuzeigen.")
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
