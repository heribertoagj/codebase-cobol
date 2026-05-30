      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0614.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0614                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: NELSON TANAKA - ALTRAN.                      *
      *     DATA........: 21/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA CONSULTA OPERACAO - AVALISTAS          *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFNTM....: LISTA CONSULTA OPERACAO - AVALISTAS          *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM5528 - OBTER OPERACOES POR CONVENIO                    *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0613                  CHAMA: DCOM0615    *
      *                                                    DCOM0616    *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT                         DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT                   IS COMMA.
      *
      *================================================================*
       DATA                                DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0614 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
       77  WRK-VOPER-E                     PIC ZZZ.ZZZ.ZZ9,99.
      *
      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                         PIC S9(08) COMP VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                         PIC S9(08) COMP VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                       PIC S9(08) COMP VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                       PIC S9(08) COMP VALUE +49369.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO                  PIC  X(08)      VALUE SPACES.
           05  WRK-INCONSIS                PIC  X(01)      VALUE SPACES.
      *
           05  WRK-BCO-AGE-CTA.
               10 WRK-CBCO                 PIC 9(03)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CAG-BCRIA            PIC 9(05)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CCTA-BCRIA-CLI       PIC 9(13)       VALUE ZEROS.
      *
           05  WRK-CNPJ-E.
FS2511*        10 WRK-CCNPJ-CPF-E          PIC 9(09)       VALUE ZEROS.
FS2511         10 WRK-CCNPJ-CPF-E          PIC X(09)       VALUE SPACES.
               10 FILLER                   PIC X(01)       VALUE '/'.
FS2511*        10 WRK-CFLIAL-CNPJ-E        PIC 9(04)       VALUE ZEROS.
FS2511         10 WRK-CFLIAL-CNPJ-E        PIC X(04)       VALUE SPACES.
               10 FILLER                   PIC X(01)       VALUE '-'.
               10 WRK-CCTRL-CNPJ-CPF-E     PIC 9(02)       VALUE ZEROS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM7995 **'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(02)          VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(09)          VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(02)          VALUE ZEROS.
           05  WRK-SAI-MSG-RETORNO     PIC  X(79)          VALUE SPACES.
           05  WRK-SAI-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(050)     VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*
      *
      *   NOTA:
      *
      *   A NAVEGACAO DE CONSULTA DE OPERACAO EH MUITO GRANDE, PORTANTO
      * A AREA DE COMUNICACAO SERA SALVA NO DBTEMPORARIO, JUNTAMENTE
      * COM OS CHAMADORES E SUAS PAGINAS, UTILIZAR ESSA AREA COMO PADRAO
      * E QUALQUER CAMPO QUE SEJA ADICIONADO NESSA AREA DEVERA SER COLO-
      * CADO APOS AO ULTIMO CAMPO.

       COPY I#DCOMJ2.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO DCOM0603 ***'.
      *----------------------------------------------------------------*
       01          COMU-AREA-IMS.
         05        COMU-LL                 PIC S9(04) COMP VALUE +406.
         05        COMU-ZZ                 PIC S9(04) COMP VALUE ZEROS.
         05        COMU-TRANCODE.
            10     COMU-TRANSACAO          PIC X(008)      VALUE SPACES.
            10     FILLER                  PIC X(006)      VALUE SPACES.
            10     COMU-PFK                PIC X(001)      VALUE SPACES.
         05        COMU-SENHAS             PIC X(037)      VALUE SPACES.
         05        COMU-COMANDO            PIC X(068)      VALUE SPACES.
         05        COMU-DADOS.
            10     COMU-TIMESTAMP          PIC X(026)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK                 PIC S9(04) COMP VALUE +124.
           05  COMU-ZZ-PFK                 PIC S9(04) COMP VALUE ZEROS.
           05  COMU-TRANCODE-PFK.
               10  COMU-TRANSACAO-PFK      PIC  X(08)      VALUE SPACES.
               10  FILLER                  PIC  X(06)      VALUE SPACES.
               10  COMU-PFK-PFK            PIC  X(01)      VALUE SPACES.
           05  COMU-SENHAS-PFK             PIC  X(37)      VALUE SPACES.
           05  COMU-COMANDO-PFK            PIC  X(68)      VALUE SPACES.
      *
      *---------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA                   PIC  X(136)     VALUE SPACES.
           05  WRK-GU                      PIC  X(04)      VALUE 'GU  '.
           05  WRK-CHNG                    PIC  X(04)      VALUE 'CHNG'.
           05  WRK-ISRT                    PIC  X(04)      VALUE 'ISRT'.
           05  WRK-FUNCAO                  PIC  X(04)      VALUE SPACES.
           05  WRK-TELA                    PIC  X(08)      VALUE SPACES.
           05  WRK-VERSAO                  PIC  X(06)      VALUE
                                                           'VRS002'.
           05  WRK-COD-USER                PIC  X(07)      VALUE SPACES.
           05  WRK-COD-USER-R              REDEFINES
               WRK-COD-USER                PIC  9(07).
           05  WRK-COD-DEPTO               PIC  9(06)      VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER                  PIC  X(04)      VALUE SPACES.
               10  WRK-TRANSACAO           PIC  X(08)      VALUE SPACES.
               10  FILLER                  PIC  X(1988)    VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFNTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFNTM.
           05  WRK-660-LL-AREA-FNTM        PIC  9(04) COMP VALUE ZEROS.
           05  WRK-660-LL-MENS-FNTM        PIC  9(04) COMP VALUE ZEROS.
           05  WRK-660-SENHAS-FNTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FNTM        PIC  9(04) COMP VALUE 2070.
           05  WRK-660-TMSTAMP-FNTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-NCLIE-FNTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CCNPJ-FNTM          PIC  9(04) COMP VALUE 0019.
           05  WRK-660-CAGCT-FNTM          PIC  9(04) COMP VALUE 0023.
           05  WRK-660-CPROD-FNTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NPROD-FNTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-CSUBP-FNTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NSUBP-FNTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-COPER-FNTM          PIC  9(04) COMP VALUE 0013.
           05  WRK-660-NSITU-FNTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-NSIT2-FNTM          PIC  9(04) COMP VALUE 0009.
           05  WRK-660-CSITU-FNTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-600-LISTA               OCCURS  04.
               10 WRK-660-SEL-FNTM         PIC  9(04) COMP VALUE 2003.
               10 WRK-660-LIT1-FNTM        PIC  9(04) COMP VALUE 0009.
               10 WRK-660-CNPJ1-FNTM       PIC  9(04) COMP VALUE 0009.
               10 WRK-660-BARRA-FNTM       PIC  9(04) COMP VALUE 0001.
               10 WRK-660-CNPJ2-FNTM       PIC  9(04) COMP VALUE 0004.
               10 WRK-660-TRACO-FNTM       PIC  9(04) COMP VALUE 0001.
               10 WRK-660-CNPJ3-FNTM       PIC  9(04) COMP VALUE 0002.
               10 WRK-660-LIT2-FNTM        PIC  9(04) COMP VALUE 0005.
               10 WRK-660-NOME-FNTM        PIC  9(04) COMP VALUE 0035.
           05  WRK-660-SALVA-FNTM          PIC  9(04) COMP VALUE 0079.
           05  WRK-660-MENSA-FNTM          PIC  9(04) COMP VALUE 0079.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05  FILLER                      PIC  X(45)      VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                      PIC  X(12)      VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE             PIC  9(02)      VALUE ZEROS.
           05  FILLER                      PIC  X(11)      VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO              PIC  9(02)      VALUE ZEROS.
           05  FILLER                      PIC  X(02)      VALUE ' *'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMFNTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FNTM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FNTM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FNTM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FNTM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FNTM.
               10  WRK-INP-TMSTAMP-FNTM    PIC  X(26)      VALUE SPACES.
      *
               10  WRK-INP-NCLIE-FNTM      PIC  X(40)      VALUE SPACE.
               10  WRK-INP-CCNPJ-FNTM      PIC  X(19)      VALUE SPACE.
               10  WRK-INP-CAGCT-FNTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-INP-CPROD-FNTM-X.
                   15  WRK-INP-CPROD-FNTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NPROD-FNTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-INP-CSUBP-FNTM-X.
                   15  WRK-INP-CSUBP-FNTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NSUBP-FNTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-INP-COPER-FNTM-X.
                   15  WRK-INP-COPER-FNTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-INP-NSITU-FNTM      PIC  X(15)      VALUE SPACE.
               10  WRK-INP-NSIT2-FNTM      PIC  X(09)      VALUE SPACES.
               10  WRK-INP-CSITU-FNTM-X.
                   15  WRK-INP-CSITU-FNTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-INP-LINHAS-FNTM OCCURS 04 TIMES.
                   15  WRK-INP-SELEC-FNTM
                                           PIC  X(01)      VALUE SPACES.
                   15  WRK-INP-LIT1-FNTM   PIC  X(09)      VALUE SPACES.
                   15  WRK-INP-CNPJ1-FNTM.
                       20  WRK-INP-CNPJ1-FNTM-N
                                           PIC  9(09)      VALUE ZEROS.
                   15  WRK-INP-BARRA-FNTM  PIC  X(01)      VALUE SPACES.
                   15  WRK-INP-CNPJ2-FNTM.
                       20  WRK-INP-CNPJ2-FNTM-N
                                           PIC  9(04)      VALUE ZEROS.
                   15  WRK-INP-TRACO-FNTM  PIC  X(01)      VALUE SPACES.
                   15  WRK-INP-CNPJ3-FNTM.
                       20  WRK-INP-CNPJ3-FNTM-N
                                           PIC  9(02)      VALUE ZEROS.
                   15  WRK-INP-LIT2-FNTM   PIC  X(05)      VALUE SPACES.
                   15  WRK-INP-NOME-FNTM   PIC  X(35)      VALUE SPACES.
           05  WRK-INP-SALVA-FNTM          PIC  X(79)      VALUE SPACES.
           05  WRK-INP-MENSA-FNTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFNTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-FNTM.
           05  WRK-OUT-LL-FNTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-ZZ-FNTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FNTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-FNTM   PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-COMANDO-FNTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FNTM.
               10  WRK-OUT-TMSTAMP-FNTM    PIC  X(26)      VALUE SPACES.
      *
               10  WRK-OUT-NCLIE-FNTM      PIC  X(40)      VALUE SPACE.
               10  WRK-OUT-CCNPJ-FNTM      PIC  X(19)      VALUE SPACE.
               10  WRK-OUT-CAGCT-FNTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-OUT-CPROD-FNTM-X.
                   15  WRK-OUT-CPROD-FNTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NPROD-FNTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-OUT-CSUBP-FNTM-X.
                   15  WRK-OUT-CSUBP-FNTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NSUBP-FNTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-OUT-COPER-FNTM-X.
                   15  WRK-OUT-COPER-FNTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-OUT-NSITU-FNTM      PIC  X(15)      VALUE SPACE.
               10  WRK-OUT-NSIT2-FNTM      PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-CSITU-FNTM-X.
                   15  WRK-OUT-CSITU-FNTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-OUT-LINHAS-FNTM OCCURS 04 TIMES.
                   15  WRK-OUT-SELEC-ATTR-FNTM
                                           PIC S9(04) COMP VALUE ZEROS.
                   15  WRK-OUT-SELEC-FNTM
                                           PIC  X(01)      VALUE SPACES.
                   15  WRK-OUT-LIT1-FNTM   PIC  X(09)      VALUE SPACES.
                   15  WRK-OUT-CNPJ1-FNTM.
                       20  WRK-OUT-CNPJ1-FNTM-N
                                           PIC  9(09)      VALUE ZEROS.
                   15  WRK-OUT-BARRA-FNTM  PIC  X(01)      VALUE SPACES.
                   15  WRK-OUT-CNPJ2-FNTM.
                       20  WRK-OUT-CNPJ2-FNTM-N
                                           PIC  9(04)       VALUE ZEROS.
                   15  WRK-OUT-TRACO-FNTM  PIC  X(01)      VALUE SPACES.
                   15  WRK-OUT-CNPJ3-FNTM.
                       20  WRK-OUT-CNPJ3-FNTM-N
                                           PIC  9(02)      VALUE ZEROS.
                   15  WRK-OUT-LIT2-FNTM   PIC  X(05)      VALUE SPACES.
                   15  WRK-OUT-NOME-FNTM   PIC  X(35)      VALUE SPACES.
           05  WRK-OUT-SALVA-FNTM          PIC  X(79)      VALUE SPACES.
           05  WRK-OUT-MENSA-FNTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DO MODULO DCOM5528 ***'.
      *----------------------------------------------------------------*
      *
       01  ENT5528-ROTEADOR.
           03  ENT5528-HEADER.
               05 ENT5528-COD-RETORNO           PIC  X(004).
               05 ENT5528-MSG-RETORNO           PIC  X(079).
               05 ENT5528-RESTART               PIC  9(005).
               05 ENT5528-FLAG                  PIC  X(001).
               05 ENT5528-START-COUNT           PIC  X(001).
               05 ENT5528-CFUNC-BDSCO           PIC  9(009).
               05 ENT5528-CTERM                 PIC  X(008).
           03  ENT5528-CONSISTENCIA.
               05 ENT5528-DANO-OPER-DESC        PIC  9(004).
               05 ENT5528-NSEQ-OPER-DESC        PIC  9(009).
               05 ENT5528-CINDCD-PSSOA-DESC     PIC  X(002).
               05 ENT5528-TIMESTAMP             PIC  X(026).
               05 ENT5528-FLAG-PESQ             PIC  X(001).
               05  FILLER                       PIC  X(049).

       01  SAI5528-ROTEADOR.
           03  SAI5528-HEADER.
               05 SAI5528-COD-RETORNO           PIC  X(004).
               05 SAI5528-MSG-RETORNO           PIC  X(079).
               05 SAI5528-RESTART               PIC  9(005).
               05 SAI5528-FLAG                  PIC  X(001).
               05 SAI5528-QTDE-RETORNADA        PIC  9(003).
               05 SAI5528-COUNT                 PIC  9(009).
           03  SAI5528-DADOS.
               05  SAI5528-TABELA-SAIDA          OCCURS 015 TIMES.
                   07 SAI5528-NRELAC-PSSOA-DESC    PIC 9(09).
FS2511*            07 SAI5528-CCNPJ-CPF            PIC 9(09).
FS2511             07 SAI5528-CCNPJ-CPF            PIC X(09).
FS2511*            07 SAI5528-CFLIAL-CNPJ          PIC 9(05).
FS2511             07 SAI5528-CFLIAL-CNPJ          PIC X(04).
                   07 SAI5528-CCTRL-CNPJ-CPF       PIC 9(02).
                   07 SAI5528-CDOCTO-ID            PIC X(15).
                   07 SAI5528-IPSSOA-DESC-COML     PIC X(60).
                   07 SAI5528-CCLUB                PIC 9(10).
                   07 SAI5528-CINDCD-PSSOA-DESC    PIC X(02).
                   07 SAI5528-CBCO                 PIC 9(03).
                   07 SAI5528-CAG-BCRIA            PIC 9(05).
                   07 SAI5528-CCTA-BCRIA-CLI       PIC 9(13).
                   07 SAI5528-CPOSTO-SERVC         PIC 9(05).
                   07 SAI5528-IPSSOA-CONJG         PIC X(40).
                   07 SAI5528-CCPF-CONJG           PIC 9(09).
                   07 SAI5528-CCTRL-CPF-CONJG      PIC 9(02).
                   07 SAI5528-CDOCTO-ID-CONJG      PIC X(15).
                   07 SAI5528-CINDCD-CONJG-AUTRZ   PIC X(01).
                   07 SAI5528-CCEP                 PIC 9(05).
                   07 SAI5528-CCEP-COMPL           PIC 9(03).
                   07 SAI5528-ELOGDR               PIC X(40).
                   07 SAI5528-IBAIRRO              PIC X(20).
                   07 SAI5528-IMUN                 PIC X(30).
                   07 SAI5528-CSGL-UF              PIC X(02).
                   07 SAI5528-DINCL-REG            PIC X(10).
                   07 SAI5528-HULT-ATULZ           PIC X(26).
                   07 SAI5528-CFUNC-BDSCO          PIC 9(09).
                   07 SAI5528-CTERM                PIC X(08).
           03  SAI5528-CONSISTENCIA  REDEFINES   SAI5528-DADOS.
               05  SAI5528-TAMANHO-ERRO          PIC  9(001).
               05  SAI5528-TABELA-ERROS          OCCURS 004 TIMES.
                   07 SAI5528-CAMPOS-ERROS      PIC  9(001).

      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY I#DCOMSY.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY I#BRAD7C.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0614 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
      *
       01  LNK-IO-PCB.
           05 LNK-IO-LTERM                 PIC  X(08).
           05 FILLER                       PIC  X(02).
           05 LNK-IO-STATUS                PIC  X(02).
           05 FILLER                       PIC  X(12).
           05 LNK-IO-MODNAME               PIC  X(08).
      *
       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM                PIC  X(08).
           05 FILLER                       PIC  X(02).
           05 LNK-ALT-STATUS               PIC  X(02).
           05 FILLER                       PIC  X(12).
           05 LNK-ALT-MODNAME              PIC  X(08).
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE WRK-GU                     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF  RETURN-CODE                 EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                     TO WRK-OUT-DADOS-FNTM
                                              WRK-OUT-SENHAS-FNTM
                                              WRK-OUT-COMANDO-FNTM
                                              COMU-DADOS.

           INITIALIZE                         ENT5528-ROTEADOR
                                              SAI5528-ROTEADOR.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMFNTM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0614'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFNTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFNTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO COMU-AREA-IMS
           MOVE COMU-TIMESTAMP             TO WRK-INP-TMSTAMP-FNTM
                                              WRK-OUT-TMSTAMP-FNTM
                                              WRK-ENT-TIMESTAMP.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-DANO-OPER-DESC   TO WRK-OUT-COPER-FNTM-X(1:4).
           MOVE WRK-NSEQ-OPER-DESC   TO WRK-OUT-COPER-FNTM-X(5:9).
           MOVE WRK-NOMECLI          TO WRK-OUT-NCLIE-FNTM.
           MOVE WRK-CNPJCPF          TO WRK-OUT-CCNPJ-FNTM.

           STRING  '237/' WRK-AGENCIA '/'
                   WRK-CONTA         DELIMITED BY SIZE
                                     INTO WRK-OUT-CAGCT-FNTM

           MOVE WRK-SITU1                  TO WRK-OUT-NSITU-FNTM.
           MOVE WRK-SITU2                  TO WRK-OUT-NSIT2-FNTM.
           MOVE WRK-SITU3                  TO WRK-OUT-CSITU-FNTM-X.

           MOVE WRK-PRODUTO                TO WRK-OUT-CPROD-FNTM.
           MOVE WRK-DESC-PRODUTO           TO WRK-OUT-NPROD-FNTM.
           MOVE WRK-SUBPRODUTO             TO WRK-OUT-CSUBP-FNTM.
           MOVE WRK-DESC-SPROD             TO WRK-OUT-NSUBP-FNTM.

           INITIALIZE  ENT5528-ROTEADOR
                       SAI5528-ROTEADOR.

           PERFORM 7000-ACESSAR-DCOM5528.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0614'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           IF  WRK-OUT-LIT1-FNTM(1)   EQUAL SPACES
                   MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11> P/
      -                 'CONTINUAR OU <PF3> P/ RETORNAR'  TO
                                              WRK-OUT-MENSA-FNTM
                   MOVE WRK-49361         TO  WRK-OUT-COMANDO-ATTR-FNTM
               ELSE
                   MOVE 'SELECIONE UMA OPCAO COM "X" OU TECLE <PF11> PAR
      -                 'A CONTINUAR'    TO  WRK-OUT-MENSA-FNTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8200-ACESSAR-DCOM7995           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE 'DCOM0601'             TO WRK-ENT-TRANS

           MOVE 'DCOM7995'             TO WRK-MODULO.

           CALL WRK-MODULO          USING WRK-ENTRADA-DCOM7995
                                          WRK-SAIDA-DCOM7995
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       8200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                    SECTION.
      *----------------------------------------------------------------*

           IF (SAI5528-CCNPJ-CPF(WRK-IND) NOT NUMERIC) OR
              (SAI5528-CCNPJ-CPF(WRK-IND) EQUAL ZEROS)
               MOVE WRK-225
                 TO WRK-OUT-SELEC-ATTR-FNTM(WRK-IND)
           ELSE
               MOVE 'CNPJ/CPF:'        TO WRK-OUT-LIT1-FNTM(WRK-IND)
               MOVE '/'                TO WRK-OUT-BARRA-FNTM(WRK-IND)
               MOVE '-'                TO WRK-OUT-TRACO-FNTM(WRK-IND)
               MOVE 'NOME:'            TO WRK-OUT-LIT2-FNTM(WRK-IND)
               MOVE SAI5528-CCNPJ-CPF (WRK-IND)
                                       TO WRK-OUT-CNPJ1-FNTM-N(WRK-IND)
               MOVE SAI5528-CFLIAL-CNPJ(WRK-IND)
                                       TO WRK-OUT-CNPJ2-FNTM-N(WRK-IND)
               MOVE SAI5528-CCTRL-CNPJ-CPF(WRK-IND)
                                       TO WRK-OUT-CNPJ3-FNTM-N(WRK-IND)
               MOVE SAI5528-IPSSOA-DESC-COML(WRK-IND)
                                       TO WRK-OUT-NOME-FNTM(WRK-IND)

           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2115-LIMPAR-TELA                    SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-225                TO
               WRK-OUT-SELEC-ATTR-FNTM(WRK-IND).
           MOVE SPACES                 TO WRK-OUT-CNPJ1-FNTM(WRK-IND)
                                          WRK-OUT-CNPJ2-FNTM(WRK-IND)
                                          WRK-OUT-CNPJ3-FNTM(WRK-IND)
                                          WRK-OUT-NOME-FNTM(WRK-IND)
                                          WRK-OUT-TRACO-FNTM(WRK-IND)
                                          WRK-OUT-BARRA-FNTM(WRK-IND).

      *----------------------------------------------------------------*
       2115-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFNTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFNTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FNTM

           PERFORM 2205-DEVOLVER-TELA-FIXO

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FNTM         EQUAL 'H' OR
                  (WRK-INP-PFK-FNTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FNTM    EQUAL 'PFK01')
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-INP-MENSA-FNTM TO  WRK-OUT-MENSA-FNTM

             WHEN WRK-INP-PFK-FNTM         EQUAL '3' OR
                   (WRK-INP-PFK-FNTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FNTM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FNTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FNTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FNTM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FNTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FNTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FNTM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

             WHEN WRK-INP-PFK-FNTM         EQUAL 'Y' OR
                 (WRK-INP-PFK-FNTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FNTM     EQUAL 'PFK11')
                  PERFORM 2272-TRANSFERIR-CONTROLE-PFK11

             WHEN (WRK-INP-PFK-FNTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FNTM    EQUAL SPACES)
                   PERFORM 2270-TRATAR-ENTER
                   PERFORM 2210-DEVOLVER-TELA-VARIA

             WHEN  OTHER
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FNTM
                   PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

           IF  WRK-OUT-LIT1-FNTM(1)   EQUAL SPACES
               MOVE WRK-49361         TO  WRK-OUT-COMANDO-ATTR-FNTM
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-OUT-TMSTAMP-FNTM
           MOVE WRK-INP-SALVA-FNTM         TO WRK-OUT-SALVA-FNTM.
           MOVE WRK-INP-MENSA-FNTM         TO WRK-OUT-MENSA-FNTM.
           MOVE WRK-INP-NCLIE-FNTM         TO WRK-OUT-NCLIE-FNTM
           MOVE WRK-INP-CCNPJ-FNTM         TO WRK-OUT-CCNPJ-FNTM
           MOVE WRK-INP-CAGCT-FNTM         TO WRK-OUT-CAGCT-FNTM
           MOVE WRK-INP-CPROD-FNTM-X       TO WRK-OUT-CPROD-FNTM-X
           MOVE WRK-INP-NPROD-FNTM         TO WRK-OUT-NPROD-FNTM
           MOVE WRK-INP-CSUBP-FNTM-X       TO WRK-OUT-CSUBP-FNTM-X
           MOVE WRK-INP-NSUBP-FNTM         TO WRK-OUT-NSUBP-FNTM
           MOVE WRK-INP-COPER-FNTM-X       TO WRK-OUT-COPER-FNTM-X
           MOVE WRK-INP-NSITU-FNTM         TO WRK-OUT-NSITU-FNTM
           MOVE WRK-INP-NSIT2-FNTM         TO WRK-OUT-NSIT2-FNTM
           MOVE WRK-INP-CSITU-FNTM-X       TO WRK-OUT-CSITU-FNTM-X.

      *----------------------------------------------------------------*
       2205-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS VARIAVEIS                            *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA            SECTION.
      *----------------------------------------------------------------*


           PERFORM  VARYING  WRK-IND  FROM 1 BY 1
                      UNTIL  WRK-IND  GREATER 04

               IF (WRK-INP-CNPJ1-FNTM  (WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-CNPJ1-FNTM-N(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-FNTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-FNTM(WRK-IND)
                                     TO WRK-OUT-SELEC-FNTM(WRK-IND)
                   MOVE WRK-INP-CNPJ1-FNTM(WRK-IND)
                                     TO WRK-OUT-CNPJ1-FNTM(WRK-IND)
                   MOVE WRK-INP-CNPJ2-FNTM(WRK-IND)
                                     TO WRK-OUT-CNPJ2-FNTM(WRK-IND)
                   MOVE WRK-INP-CNPJ3-FNTM(WRK-IND)
                                     TO WRK-OUT-CNPJ3-FNTM(WRK-IND)
                   MOVE WRK-INP-LIT1-FNTM(WRK-IND)
                                     TO WRK-OUT-LIT1-FNTM(WRK-IND)
                   MOVE WRK-INP-LIT2-FNTM(WRK-IND)
                                     TO WRK-OUT-LIT2-FNTM(WRK-IND)
                   MOVE WRK-INP-TRACO-FNTM(WRK-IND)
                                     TO WRK-OUT-TRACO-FNTM(WRK-IND)
                   MOVE WRK-INP-BARRA-FNTM(WRK-IND)
                                     TO WRK-OUT-BARRA-FNTM(WRK-IND)
                   MOVE WRK-INP-NOME-FNTM(WRK-IND)
                                     TO WRK-OUT-NOME-FNTM(WRK-IND)

               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0610                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*


           INITIALIZE COMU-AREA-IMS
           MOVE SPACES                     TO COMU-DADOS

           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA-IMS       TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0614'                 TO COMU-TRANSACAO
           MOVE '3'                        TO COMU-PFK
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2230-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0614'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE '5'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2230-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2240-MENU-DCOM                      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0614'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                    TO WRK-INCONSIS

           PERFORM 2271-CONSISTIR-FISICA.

      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                      TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 04

               IF  WRK-INP-SELEC-FNTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'                TO WRK-INCONSIS
                   MOVE WRK-49369          TO
                        WRK-OUT-SELEC-ATTR-FNTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)    TO WRK-OUT-MENSA-FNTM
               ELSE
                   IF  WRK-INP-SELEC-FNTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369      TO
                            WRK-OUT-SELEC-ATTR-FNTM(WRK-IND)
                       ADD 1               TO WCT-SELEC
                       MOVE WRK-IND        TO WIND-SEL
                   END-IF
               END-IF
           END-PERFORM.

           IF WRK-INCONSIS  EQUAL  '*'
              GO TO 2271-99-FIM
           END-IF.

           IF  WCT-SELEC                   EQUAL 0  OR  GREATER 1
               MOVE '*'                    TO WRK-INCONSIS
               IF  WCT-SELEC               EQUAL 0
                   MOVE SG-MENSAGEM(53)    TO WRK-OUT-MENSA-FNTM
               ELSE
                 IF  WCT-SELEC             GREATER 1
                     MOVE SG-MENSAGEM(39)  TO WRK-OUT-MENSA-FNTM
                 END-IF
               END-IF
           ELSE
               PERFORM 2273-TRANSFERIR-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2273-TRANSFERIR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL


           MOVE WRK-INP-CNPJ1-FNTM-N(WIND-SEL)
                                           TO WRK-CCNPJ-AVAL
           MOVE WRK-INP-CNPJ2-FNTM-N(WIND-SEL)
                                           TO WRK-FILIAL-AVAL
           MOVE WRK-INP-CNPJ3-FNTM-N(WIND-SEL)
                                           TO WRK-CCTRL-AVAL
           MOVE WRK-INP-NOME-FNTM  (WIND-SEL)
                                           TO WRK-NOME-AVAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0614'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.


           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL.
           MOVE 'DCOM0614'                 TO COMU-TRANSACAO
           MOVE 'DCOM0615'                 TO WRK-TELA
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2273-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE-PFK11      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FNTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-INP-CNPJ1-FNTM-N(WIND-SEL)
                                           TO WRK-CCNPJ-AVAL
           MOVE WRK-INP-CNPJ2-FNTM-N(WIND-SEL)
                                           TO WRK-FILIAL-AVAL
           MOVE WRK-INP-CNPJ3-FNTM-N(WIND-SEL)
                                           TO WRK-CCTRL-AVAL
           MOVE WRK-INP-NOME-FNTM(WIND-SEL)
                                           TO WRK-NOME-AVAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0614'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL.
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE 'DCOM0614'                 TO COMU-TRANSACAO
           MOVE 'DCOM0616'                 TO WRK-TELA
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FNTM
                                       TO  WRK-660-LL-MENS-FNTM
                                           WRK-OUT-LL-FNTM

           MOVE LENGTH OF WRK-660-DCOMFNTM
                                       TO  WRK-660-LL-AREA-FNTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FNTM
                                              WRK-660-DCOMFNTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FNTM            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3000-FINALIZAR                      SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       7000-ACESSAR-DCOM5528               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                        TO ENT5528-START-COUNT
           MOVE ZEROS                      TO ENT5528-FLAG
           MOVE ZEROS                      TO ENT5528-RESTART
           MOVE WRK-COD-USER               TO ENT5528-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5528-CTERM
           MOVE WRK-DANO-OPER-DESC         TO ENT5528-DANO-OPER-DESC
           MOVE WRK-NSEQ-OPER-DESC         TO ENT5528-NSEQ-OPER-DESC
           MOVE 'T'                        TO ENT5528-FLAG-PESQ
           MOVE 'AV'                       TO ENT5528-CINDCD-PSSOA-DESC
           MOVE 'DCOM5528'                 TO WRK-MODULO


           CALL WRK-MODULO             USING  ENT5528-ROTEADOR
                                              SAI5528-ROTEADOR
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5528-COD-RETORNO
              WHEN '0000'
              WHEN '0002'

                PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 04

              WHEN '0003'
              WHEN '0100'
                MOVE SG-MENSAGEM(63)       TO WRK-OUT-MENSA-FNTM

                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 04

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                MOVE SAI5528-MSG-RETORNO   TO WRK-OUT-MENSA-FNTM
                PERFORM 2115-LIMPAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 04

           END-EVALUATE

           .
      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0614'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0614'             TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER               TO ERR-COD-USER
           MOVE WRK-COD-DEPTO              TO ERR-COD-DEPTO

           IF  ERR-TIPO-ACESSO             EQUAL 'DB2'
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
