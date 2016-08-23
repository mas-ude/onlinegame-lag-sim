\ProvidesClass{UDEbeamerEN}[08/17/2016 v1.0-mas0.3 Englische Vorlage fuer Universitaet Duisburg-Essen]
% aktualisiert durch Florian Metzger, April 2015
% Änderungen:
% UTF8
% Untertitel
% pdf logo und damit pdflatex Kompatibilität
% \titlepage Unterstützung (nicht nur \maketitle)


%Wichtige Optionen zur Funktionstüchtigkeit
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{beamer}}
\ProcessOptions \relax
\LoadClass[xcolor=table,8pt,covered,hyperref={bookmarks,colorlinks}]{beamer}%PPT-Folien sind nahezu doppelt so groß wie Standard-Beamerfolien, weswegen die Standardschriftgröße n Beamer (hier 8pt) der Hälfte in PPT (16pt beim UDE-Format) entsprechen muss

%Definition von Schriftart und Zeichensatz
\renewcommand{\baselinestretch}{1.50}
\RequirePackage{setspace}
\RequirePackage{graphicx}
\RequirePackage[scaled]{helvet}

%Definition der UDE-Farben
\definecolor{UDEgelb}{RGB}{239,228,191}
\definecolor{UDEblau}{RGB}{0,76,147}

%Definition des Randes
\newlength{\rand}
\setlength{\rand}{0.25cm}
\newlength{\doppelrand}
\setlength{\doppelrand}{0.5cm}
\newlength{\lengthleft}
\setlength{\lengthleft}{\paperwidth}
\addtolength{\lengthleft}{-\doppelrand}
\newlength{\einzugminusrand}
\setlength{\einzugminusrand}{5pt}
\newlength{\einzug}
\setlength{\einzug}{\rand}
\addtolength{\einzug}{\einzugminusrand}

\setbeamersize{text margin left=\einzug, text margin right=\rand}
\setbeamertemplate{headline}{\vspace{\rand}}

%Definition der Farben von Beamer-Elementen
\setbeamercolor*{frametitle}{fg=UDEblau,bg=UDEgelb}
\setbeamercolor*{footline}{fg=UDEblau,bg=white}
\setbeamercolor*{title}{fg=UDEblau,bg=white}
\setbeamercolor*{topic}{fg=UDEblau,bg=white}
\setbeamercolor*{author}{fg=UDEblau,bg=white}
\setbeamercolor*{date}{fg=UDEblau,bg=white}
\setbeamercolor{itemize item}{fg=UDEblau}
\setbeamercolor{UDEBoxgelb}{bg=UDEgelb}
\setbeamercolor*{separation line}{fg=UDEblau}

%Definition der Schriftartdetails
\setbeamerfont{title}{size=\Huge,series=\bfseries\itshape}
\setbeamerfont{topic}{size=\Large}
\setbeamerfont*{author}{size=\normalsize}
\setbeamerfont*{date}{size=\normalsize}
\setbeamerfont*{square}{size=\tiny}
\setbeamerfont{frametitle}{size=\large,series=\bfseries}
\setbeamerfont*{footline}{size=\footnotesize}

%Definition anderer Standardeinstellungen
\setbeamertemplate{itemize item}[square]
\setbeamertemplate{itemize subitem}[square]
\setbeamertemplate{itemize subsubitem}[square]
%\setbeamertemplate{itemize item}{\small\raise1.25pt}
\setbeamertemplate*{headline}{}
\setbeamertemplate{navigation symbols}{}
\beamerdefaultoverlayspecification{}
\def\beamer@andinst{\quad}

%Definition zusätzlicher Angaben für die Fusszeile und Titelseite
\mode
<all>
{\newcommand\webadresse[1]{\def\insertwebadresse{#1}}
\webadresse{}}
\mode
<all>
{\newcommand\lehrstuhl[1]{\def\insertlehrstuhl{#1}}
\lehrstuhl{}}
\mode
<all>
{\newcommand\topic[1]{\def\inserttopic{#1}}
\topic{}}

%Definition der UDE-Logos
\pgfdeclareimage[height=0.86cm,width=2.225cm]{ude_logo}{logo_claim_en.pdf}
\pgfdeclareimage[height=1.985cm]{mas_logo}{logo-nontransparent.pdf}
\pgfdeclareimage[height=0.2cm]{mas_logo_footline}{logo-nontransparent.pdf}
\pgfdeclareimage[height=0.86cm]{mas_logo_headline}{logo-nontransparent.pdf}
\pgfdeclareimage[height=1.985cm,width=5.14cm]{ude_logo_titelseite}{logo_claim_en.pdf}

%Definition eines Templates für Titelseite
\setlength{\fboxsep}{0pt}


\defbeamertemplate*{title page}{UDE}{%
	\hspace{-\einzugminusrand}
	\begin{beamercolorbox}[wd=\lengthleft,ht=5.72cm]{UDEBoxgelb}%
		\pgfuseimage{mas_logo} \hfill \pgfuseimage{ude_logo_titelseite}
		\vspace{1.24cm}
	\end{beamercolorbox}\par
	\vspace{0.25cm}\par
	\hspace{-\einzugminusrand}\fcolorbox{UDEblau}{white}{
		 \begin{beamercolorbox}[wd=\lengthleft,ht=3.055cm,sep=5pt,left]{white}%
		 	\hspace{\einzugminusrand}%
			\begin{spacing}{0.8}
				\parbox{0.96\lengthleft}{\usebeamerfont{title}\usebeamercolor[fg]{title}\inserttitle}\newline
			\end{spacing}
			\vspace{1ex}
			{%
				\ifx\insertsubtitle\@empty%
				\else%
					{\usebeamerfont{topic}\usebeamercolor[fg]{topic}\insertsubtitle \newline}%
				\fi
			}%
			\usebeamerfont{author}\insertauthor \usebeamerfont{square}$\text{ }\blacksquare$ \usebeamerfont{date}\insertdate\newline%
			{\small{\textit\insertlehrstuhl}\hfill\hspace{5cm}\insertwebadresse}
			
		\end{beamercolorbox}\par
	}
}

%Definition eines Templates für normale Seiten
\defbeamertemplate{frametitle}{UDE}{
	\hspace{-\einzugminusrand} 
	\begin{beamercolorbox}[wd=\lengthleft,ht=1.25cm,dp=0pt,leftskip=\einzugminusrand]{UDEBoxgelb}
		\vspace{0cm}
		\parbox[1.25cm]{0.08\lengthleft}{\pgfuseimage{mas_logo_headline}}
		\parbox[c][1.25cm]{0.65\lengthleft}{%
			\begin{spacing}{1.1}
				\usebeamerfont*{frametitle}\usebeamercolor[fg]{frametitle}\insertframetitle\strut\par%
				{%
				\ifx\insertframesubtitle\@empty%
				\else%
					{\usebeamerfont{framesubtitle}\usebeamercolor[fg]{framesubtitle}\insertframesubtitle\strut\par}%
				\fi
				}%
			\end{spacing}	
		}
		%\vspace{3pt}
		\hfill
		%
		\parbox{0.2\lengthleft}{\hfill\pgfuseimage{ude_logo}}
	\end{beamercolorbox}\par
	\vspace{-0.25cm}
	 {\hspace{-\einzugminusrand}\usebeamercolor{separation line}\rule{\lengthleft}{0.375pt}}
}

%Definition des Fusszeilentemplates
\newcommand{\UDE@footline}{
	\parbox{0.333\linewidth}{\hspace{5pt}\insertlehrstuhl}
	\parbox{0.333\linewidth}{\centering  \insertdate \par}
	\parbox{0.333\linewidth}{\raggedleft \insertframenumber/\inserttotalframenumber}
}

\defbeamertemplate{footline}{UDE}{%
	\ifnum \insertpagenumber=1
	\else
		\begin{beamercolorbox}[wd=\textwidth,ht=1ex,leftskip=\einzugminusrand,rightskip=\rand]{white}
		\vspace{2ex}
		 {\hspace{-\einzugminusrand}\usebeamercolor{separation line}\rule{\lengthleft}{0.375pt}}
			\usebeamerfont{footline}\UDE@footline
			\vspace{2ex}
		\end{beamercolorbox}%
	\fi
}

%Passe \maketitle an
\def\maketitle{\ifbeamer@inframe\titlepage\else\frame{\titlepage}\fi}

\setbeamertemplate{frametitle}[UDE]\setbeamertemplate{footline}[UDE]










