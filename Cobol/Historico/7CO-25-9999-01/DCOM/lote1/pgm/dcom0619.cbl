      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0619.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0619                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: NELSON TANAKA - ALTRAN.                      *
      *     DATA........: 21/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA CONSULTA OPERACAO - MEIO DE LIBERACAO  *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFSTM....: LISTA CONSULTA OPERACAO - MEIO DE LIBERACAO  *
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
      *     DCOM5527 - MODULO DE ACESSO                                *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0618                  CHAMA: DCOM0620    *
      *================================================================*
BRQ001*                   ALTERACAO  -  BRQ DIGITAL SOLUTIONS          *
BRQ001*----------------------------------------------------------------*
BRQ001*    PROGRAMADOR.:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ001*    ANALISTA BRQ:  FABIO AUGUSTO FINK - BRQ DIGITAL SOLUTIONS   *
BRQ001*    ANALISTA DS.:  RICARDO JAMMAL     - BRADESCO / GP.39        *
BRQ001*    DATA........:  26/11/2018                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*    OBJETIVO....:  BRQ 18/0456 - BORDERO ELETRONICO.            *
BRQ001*      INCLUIR TRATAMENTO PARA CAMPO INDICADOR DE LIBERACAO      *
BRQ001*      NO CANAL.                                                 *
BRQ001*================================================================*
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
       77  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0619 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
       77  WRK-VOPER-E                     PIC Z.ZZZ.ZZZ.ZZ9,99.
       77  WRK-PERC-E                      PIC ZZ9,99.
      *
      *---------------------------------------------------------------*
       77  FILLER                          PIC  X(50)      VALUE
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
           05  WRK-CNPJ-E.
FS2511*        10 WRK-CCNPJ-CPF-E          PIC 9(09)       VALUE ZEROS.
FS2511         10 WRK-CCNPJ-CPF-E          PIC X(09)       VALUE SPACES.
               10 FILLER                   PIC X(01)       VALUE '/'.
FS2511*        10 WRK-CFLIAL-CNPJ-E        PIC 9(04)       VALUE ZEROS.
FS2511         10 WRK-CFLIAL-CNPJ-E        PIC X(04)       VALUE SPACES.
               10 FILLER                   PIC X(01)       VALUE '-'.
               10 WRK-CCTRL-CNPJ-CPF-E     PIC 9(02)       VALUE ZEROS.
      *
           05  WRK-BCO-AGE-CTA.
               10 WRK-CBCO                 PIC 9(03)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CAG-BCRIA            PIC 9(05)       VALUE ZEROS.
               10 FILLER                   PIC X(01)       VALUE '/'.
               10 WRK-CCTA-BCRIA-CLI       PIC 9(13)       VALUE ZEROS.
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
            10     COMU-MENSAGEM           PIC X(079)      VALUE SPACES.
      *
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
           '*** AREA DA TELA DCOMFSTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFSTM.
           05  WRK-660-LL-AREA-FSTM        PIC  9(04) COMP VALUE 0000.
           05  WRK-660-LL-MENS-FSTM        PIC  9(04) COMP VALUE 0000.
           05  WRK-660-SENHAS-FSTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FSTM        PIC  9(04) COMP VALUE 0068.
           05  WRK-660-TMSTAMP-FSTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-TIPOOPC-FSTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-TIPOCON-FSTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-ULTPAG-FSTM         PIC  9(04) COMP VALUE 0001.
           05  WRK-660-PASSO-FSTM          PIC  9(04) COMP VALUE 0001.
           05  WRK-660-CHAMADO-FSTM        PIC  9(04) COMP VALUE 0008.
           05  WRK-660-PAGINA-FSTM         PIC  9(04) COMP VALUE 0004.
           05  WRK-660-NCLIE-FSTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CCNPJ-FSTM          PIC  9(04) COMP VALUE 0019.
           05  WRK-660-CAGCT-FSTM          PIC  9(04) COMP VALUE 0023.
           05  WRK-660-CPROD-FSTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NPROD-FSTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-CSUBP-FSTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NSUBP-FSTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-COPER-FSTM          PIC  9(04) COMP VALUE 0013.
           05  WRK-660-NSITU-FSTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-NSIT2-FSTM          PIC  9(04) COMP VALUE 0009.
           05  WRK-660-CSITU-FSTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-600-LISTA               OCCURS  04.
               10 WRK-660-CTPLB-FSTM       PIC  9(04) COMP VALUE 0003.
               10 WRK-660-NTPLB-FSTM       PIC  9(04) COMP VALUE 0015.
               10 WRK-660-FAVOR-FSTM       PIC  9(04) COMP VALUE 0050.
               10 WRK-660-CAGCT-FSTM       PIC  9(04) COMP VALUE 0023.
               10 WRK-660-CCNPJ-FSTM       PIC  9(04) COMP VALUE 0017.
               10 WRK-660-PERCE-FSTM       PIC  9(04) COMP VALUE 0006.
               10 WRK-660-VOPER-FSTM       PIC  9(04) COMP VALUE 0016.
               10 WRK-660-DATAO-FSTM       PIC  9(04) COMP VALUE 0010.
           05  WRK-660-SALVA-FSTM          PIC  9(04) COMP VALUE 0079.
           05  WRK-660-MENSA-FSTM          PIC  9(04) COMP VALUE 0079.
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
           '*** AREA DA TELA DCOMFSTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FSTM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FSTM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FSTM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FSTM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FSTM.
               10  WRK-INP-TMSTAMP-FSTM    PIC  X(26)      VALUE SPACES.
               10  WRK-INP-TIPOOPC-FSTM    PIC  X(01)      VALUE SPACES.
               10  WRK-INP-TIPOCON-FSTM    PIC  X(01)      VALUE SPACES.
               10  WRK-INP-ULTPAG-FSTM     PIC  X(01)      VALUE SPACE.
               10  WRK-INP-PASSO-FSTM      PIC  X(01)      VALUE SPACE.
               10  WRK-INP-CHAMADO-FSTM    PIC  X(08)      VALUE SPACE.
               10  WRK-INP-PAGINA-FSTM-X.
                   15  WRK-INP-PAGINA-FSTM
                                           PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-INP-NCLIE-FSTM      PIC  X(40)      VALUE SPACE.
               10  WRK-INP-CCNPJ-FSTM      PIC  X(19)      VALUE SPACE.
               10  WRK-INP-CAGCT-FSTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-INP-CPROD-FSTM-X.
                   15  WRK-INP-CPROD-FSTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NPROD-FSTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-INP-CSUBP-FSTM-X.
                   15  WRK-INP-CSUBP-FSTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NSUBP-FSTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-INP-COPER-FSTM-X.
                   15  WRK-INP-COPER-FSTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-INP-NSITU-FSTM      PIC  X(15)      VALUE SPACE.
               10  WRK-INP-NSIT2-FSTM      PIC  X(09)      VALUE SPACES.
               10  WRK-INP-CSITU-FSTM-X.
                   15  WRK-INP-CSITU-FSTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-INP-LINHAS-FSTM OCCURS 04 TIMES.
                   15  WRK-INP-CTPLB-FSTM  PIC  X(03)     VALUE SPACES.
                   15  WRK-INP-NTPLB-FSTM  PIC  X(15)     VALUE SPACES.
                   15  WRK-INP-FAVOR-FSTM  PIC  X(50)     VALUE SPACES.
                   15  WRK-INP-CAGCT-FSTM-TB
                                           PIC  X(23)     VALUE SPACES.
                   15  WRK-INP-CCNPJ-FSTM-TB
                                           PIC  X(17)     VALUE SPACES.
                   15  WRK-INP-PERCE-FSTM  PIC  X(06)     VALUE SPACES.
                   15  WRK-INP-VOPER-FSTM  PIC  X(16)     VALUE SPACES.
                   15  WRK-INP-DATAO-FSTM  PIC  X(10)     VALUE SPACES.
           05  WRK-INP-SALVA-FSTM          PIC  X(79)     VALUE SPACES.
           05  WRK-INP-MENSA-FSTM          PIC  X(79)     VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFSTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FSTM.
           05  WRK-OUT-LL-FSTM             PIC S9(04) COMP VALUE +0000.
           05  WRK-OUT-ZZ-FSTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FSTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-FSTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FSTM.
               10  WRK-OUT-TMSTAMP-FSTM    PIC  X(26)      VALUE SPACES.
               10  WRK-OUT-TIPOOPC-FSTM    PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-TIPOCON-FSTM    PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-ULTPAG-FSTM     PIC  X(01)      VALUE SPACE.
               10  WRK-OUT-PASSO-FSTM      PIC  X(01)      VALUE SPACE.
               10  WRK-OUT-CHAMADO-FSTM    PIC  X(08)      VALUE SPACE.
               10  WRK-OUT-PAGINA-FSTM-X.
                   15  WRK-OUT-PAGINA-FSTM PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-OUT-NCLIE-FSTM      PIC  X(40)      VALUE SPACE.
               10  WRK-OUT-CCNPJ-FSTM      PIC  X(19)      VALUE SPACE.
               10  WRK-OUT-CAGCT-FSTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-OUT-CPROD-FSTM-X.
                   15  WRK-OUT-CPROD-FSTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NPROD-FSTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-OUT-CSUBP-FSTM-X.
                   15  WRK-OUT-CSUBP-FSTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NSUBP-FSTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-OUT-COPER-FSTM-X.
                   15  WRK-OUT-COPER-FSTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-OUT-NSITU-FSTM      PIC  X(15)      VALUE SPACE.
               10  WRK-OUT-NSIT2-FSTM      PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-CSITU-FSTM-X.
                   15  WRK-OUT-CSITU-FSTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-OUT-LINHAS-FSTM OCCURS 04 TIMES.
                   15  WRK-OUT-CTPLB-FSTM  PIC  X(03)      VALUE SPACES.
                   15  WRK-OUT-NTPLB-FSTM  PIC  X(15)      VALUE SPACES.
                   15  WRK-OUT-FAVOR-FSTM  PIC  X(50)      VALUE SPACES.
                   15  WRK-OUT-CAGCT-FSTM-TB
                                           PIC  X(23)      VALUE SPACES.
                   15  WRK-OUT-CCNPJ-FSTM-TB
                                           PIC  X(17)      VALUE SPACES.
                   15  WRK-OUT-PERCE-FSTM  PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-VOPER-FSTM  PIC  X(16)      VALUE SPACES.
                   15  WRK-OUT-DATAO-FSTM  PIC  X(10)      VALUE SPACES.
           05  WRK-OUT-SALVA-FSTM          PIC  X(79)      VALUE SPACES.
           05  WRK-OUT-MENSA-FSTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DO MODULO DCOM5527 ***'.
      *----------------------------------------------------------------*
       01  ENTRADA-ROT5527.
           03  ENT5527-HEADER.
               05  ENT5527-COD-RETORNO           PIC  X(004).
               05  ENT5527-MSG-RETORNO           PIC  X(079).
               05  ENT5527-RESTART               PIC  9(005).
               05  ENT5527-FLAG                  PIC  X(001).
               05  ENT5527-START-COUNT           PIC  X(001).
               05  ENT5527-CFUNC-BDSCO           PIC  9(009).
               05  ENT5527-CTERM                 PIC  X(008).
           03  ENT5527-CONSISTENCIA.
               05  ENT5527-DANO-OPER-DESC        PIC  9(004).
               05  ENT5527-NSEQ-OPER-DESC        PIC  9(009).
               05  ENT5527-OFIC-HIST             PIC  X(001).
               05  ENT5527-HULT-ATULZ            PIC  X(026).
260309* INCLUIDO O CAMPO ENT5527-HULT-ATULZ PARA O ACESSO A TABELA
260309* DE HISTORICO, SEMPRE QUE O CAMPO ENT5527-OFIC-HIST= 'H' DEVE
260309* SER VALORIZADO O ENT5527-HULT-ATULZ COM O TIMESTAMP DA OPERACAO

       01  SAIDA-ROT5527.
           03  SAI5527-HEADER.
               05  SAI5527-COD-RETORNO           PIC  X(004).
               05  SAI5527-MSG-RETORNO           PIC  X(079).
               05  SAI5527-RESTART               PIC  9(005).
               05  SAI5527-FLAG                  PIC  X(001).
               05  SAI5527-QTDE-RETORNADA        PIC  9(003).
               05  SAI5527-COUNT                 PIC  9(009).
           03  SAI5527-DADOS.
               05  SAI5527-PLIBRC-TOTAL          PIC  9(003)V9(2).
               05  SAI5527-TABELA-SAIDA          OCCURS 015 TIMES.
                   07 SAI5527-CMEIO-LIBRC-DESC   PIC  9(003).
                   07 SAI5527-IRSUMO-MEIO-LIBRC  PIC  X(015).
                   07 SAI5527-NOPER-MEIO-LIBRC   PIC  9(003).
                   07 SAI5527-CSIT-DESC-COML     PIC  9(003).
                   07 SAI5527-HSIT-DESC-COML     PIC  X(026).
                   07 SAI5527-CBCO               PIC  9(003).
                   07 SAI5527-CAG-BCRIA          PIC  9(005).
                   07 SAI5527-CCTA-BCRIA-CLI     PIC  9(013).
                   07 SAI5527-CPOSTO-SERVC       PIC  9(003).
FS2511*            07 SAI5527-CCNPJ-CPF          PIC  9(009).
FS2511             07 SAI5527-CCNPJ-CPF          PIC  X(009).
FS2511*            07 SAI5527-CFLIAL-CNPJ        PIC  9(005).
FS2511             07 SAI5527-CFLIAL-CNPJ        PIC  X(004).
                   07 SAI5527-CCTRL-CNPJ-CPF     PIC  9(002).
                   07 SAI5527-IPSSOA-DESC-COML   PIC  X(060).
                   07 SAI5527-CCLUB              PIC  9(010).
                   07 SAI5527-VLIBRC-DESC-COML   PIC  9(015)V9(2).
                   07 SAI5527-PLIBRC-DESC-COML   PIC  9(003)V9(3).
                   07 SAI5527-DLIBRC-DESC-COML   PIC  X(010).
                   07 SAI5527-CINDCD-LIBRC-ANTCP PIC  X(001).
                   07 SAI5527-HULT-ATULZ         PIC  X(026).
                   07 SAI5527-CFUNC-BDSCO        PIC  9(009).
                   07 SAI5527-CTERM              PIC  X(008).
BRQ001             07 WRK-S5527-CINDCD-LIBRC-CANAL
BRQ001                                           PIC  X(001).
           03  SAI5527-CONSISTENCIA  REDEFINES   SAI5527-DADOS.
               05  SAI5527-TAMANHO-ERRO          PIC  9(001).
               05  SAI5527-TABELA-ERROS          OCCURS 002 TIMES.
                   07  SAI5527-CAMPOS-ERROS      PIC  9(001).

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
      *
       COPY 'I#DCOMJ2'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0619 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FSTM
                                              WRK-OUT-SENHAS-FSTM
                                              WRK-OUT-COMANDO-FSTM
                                              COMU-DADOS.

           INITIALIZE                         ENTRADA-ROT5527
                                              SAIDA-ROT5527

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

           MOVE 'DCOMFSTM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0619'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFSTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFSTM'
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
           MOVE COMU-TIMESTAMP             TO WRK-INP-TMSTAMP-FSTM
                                              WRK-OUT-TMSTAMP-FSTM
                                              WRK-ENT-TIMESTAMP.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 2120-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-DANO-OPER-DESC   TO WRK-OUT-COPER-FSTM-X(1:4).
           MOVE WRK-NSEQ-OPER-DESC   TO WRK-OUT-COPER-FSTM-X(5:9).
           MOVE WRK-NOMECLI          TO WRK-OUT-NCLIE-FSTM.
           MOVE WRK-CNPJCPF          TO WRK-OUT-CCNPJ-FSTM.
           STRING  '237/' WRK-AGENCIA '/'
                   WRK-CONTA         DELIMITED BY SIZE
                                     INTO WRK-OUT-CAGCT-FSTM

           MOVE WRK-SITU1                  TO WRK-OUT-NSITU-FSTM.
           MOVE WRK-SITU2                  TO WRK-OUT-NSIT2-FSTM.
           MOVE WRK-SITU3                  TO WRK-OUT-CSITU-FSTM-X.

           MOVE WRK-PRODUTO                TO WRK-OUT-CPROD-FSTM.
           MOVE WRK-DESC-PRODUTO           TO WRK-OUT-NPROD-FSTM.
           MOVE WRK-SUBPRODUTO             TO WRK-OUT-CSUBP-FSTM.
           MOVE WRK-DESC-SPROD             TO WRK-OUT-NSUBP-FSTM.

           INITIALIZE  ENTRADA-ROT5527
                       SAIDA-ROT5527

           MOVE 'H'                        TO ENT5527-OFIC-HIST
           PERFORM 2130-ACESSAR-DCOM5527.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0619'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 2120-ACESSAR-DCOM7995.

           MOVE 1                          TO WRK-OUT-PAGINA-FSTM.
           MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11> PARA CONTIN
      -         'AR'                       TO WRK-OUT-MENSA-FSTM.

           IF  COMU-MENSAGEM          NOT EQUAL SPACES AND LOW-VALUES
               MOVE COMU-MENSAGEM         TO  WRK-OUT-MENSA-FSTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-ACESSAR-DCOM7995           SECTION.
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
       2120-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2130-ACESSAR-DCOM5527               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                        TO ENT5527-START-COUNT
           MOVE ZEROS                      TO ENT5527-FLAG
           MOVE ZEROS                      TO ENT5527-RESTART
           MOVE WRK-COD-USER               TO ENT5527-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5527-CTERM
           MOVE WRK-DANO-OPER-DESC         TO ENT5527-DANO-OPER-DESC
           MOVE WRK-NSEQ-OPER-DESC         TO ENT5527-NSEQ-OPER-DESC
260309     MOVE WRK-TIMESTAMP              TO ENT5527-HULT-ATULZ
           MOVE 'DCOM5527'                 TO WRK-MODULO .

           CALL WRK-MODULO             USING  ENTRADA-ROT5527
                                              SAIDA-ROT5527
                                              ERRO-AREA
                                              WRK-SQLCA .

           EVALUATE SAI5527-COD-RETORNO
              WHEN '0000'
              WHEN '0002'
                IF  SAI5527-CMEIO-LIBRC-DESC(5) EQUAL ZEROS
                    MOVE '*'               TO WRK-OUT-ULTPAG-FSTM
                ELSE
                    MOVE SPACES            TO WRK-OUT-ULTPAG-FSTM
                END-IF
                PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 04

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN '0003'
                IF  ENT5527-OFIC-HIST      EQUAL 'H'
                    INITIALIZE SAIDA-ROT5527
                    MOVE 'O'               TO ENT5527-OFIC-HIST
                    GO TO 2130-ACESSAR-DCOM5527
                END-IF

              WHEN OTHER
                MOVE SAI5527-MSG-RETORNO   TO WRK-OUT-MENSA-FSTM

           END-EVALUATE .

      *----------------------------------------------------------------*
       2130-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                    SECTION.
      *----------------------------------------------------------------*


           IF (SAI5527-CMEIO-LIBRC-DESC(WRK-IND)   NOT NUMERIC) OR
              (SAI5527-CMEIO-LIBRC-DESC(WRK-IND)   EQUAL ZEROS)
               CONTINUE
           ELSE
               MOVE SAI5527-CMEIO-LIBRC-DESC(WRK-IND)
                                         TO WRK-OUT-CTPLB-FSTM(WRK-IND)
               MOVE SAI5527-IRSUMO-MEIO-LIBRC(WRK-IND)
                                         TO WRK-OUT-NTPLB-FSTM(WRK-IND)
               MOVE SAI5527-IPSSOA-DESC-COML(WRK-IND)
                                         TO WRK-OUT-FAVOR-FSTM(WRK-IND)
               MOVE SAI5527-CBCO(WRK-IND)
                                         TO WRK-CBCO
               MOVE SAI5527-CAG-BCRIA(WRK-IND)
                                         TO WRK-CAG-BCRIA
               MOVE SAI5527-CCTA-BCRIA-CLI(WRK-IND)
                                         TO WRK-CCTA-BCRIA-CLI
               MOVE WRK-BCO-AGE-CTA      TO
                                         WRK-OUT-CAGCT-FSTM-TB(WRK-IND)
               MOVE SAI5527-CCNPJ-CPF(WRK-IND)
                                         TO WRK-CCNPJ-CPF-E
               MOVE SAI5527-CFLIAL-CNPJ(WRK-IND)
                                         TO WRK-CFLIAL-CNPJ-E
               MOVE SAI5527-CCTRL-CNPJ-CPF(WRK-IND)
                                         TO WRK-CCTRL-CNPJ-CPF-E
               MOVE WRK-CNPJ-E           TO
                                         WRK-OUT-CCNPJ-FSTM-TB(WRK-IND)
               MOVE SAI5527-PLIBRC-DESC-COML(WRK-IND)
                                         TO WRK-PERC-E
               MOVE WRK-PERC-E           TO WRK-OUT-PERCE-FSTM(WRK-IND)
               MOVE SAI5527-VLIBRC-DESC-COML(WRK-IND)
                                         TO WRK-VOPER-E
               MOVE WRK-VOPER-E          TO WRK-OUT-VOPER-FSTM(WRK-IND)
               MOVE SAI5527-DLIBRC-DESC-COML(WRK-IND)
                                         TO WRK-OUT-DATAO-FSTM(WRK-IND)
               INSPECT WRK-OUT-DATAO-FSTM(WRK-IND) REPLACING ALL '.'
                                                              BY '/'
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

           MOVE SPACES                TO WRK-OUT-CTPLB-FSTM(WRK-IND)
                                         WRK-OUT-NTPLB-FSTM(WRK-IND)
                                         WRK-OUT-FAVOR-FSTM(WRK-IND)
                                         WRK-OUT-CAGCT-FSTM-TB(WRK-IND)
                                         WRK-OUT-CCNPJ-FSTM-TB(WRK-IND)
                                         WRK-OUT-PERCE-FSTM(WRK-IND)
                                         WRK-OUT-VOPER-FSTM(WRK-IND)
                                         WRK-OUT-DATAO-FSTM(WRK-IND).

      *----------------------------------------------------------------*
       2115-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFSTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFSTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FSTM

           PERFORM 2205-DEVOLVER-TELA-FIXO

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FSTM         EQUAL 'H' OR
                  (WRK-INP-PFK-FSTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FSTM    EQUAL 'PFK01')
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-INP-MENSA-FSTM TO  WRK-OUT-MENSA-FSTM

             WHEN WRK-INP-PFK-FSTM         EQUAL '3' OR
                   (WRK-INP-PFK-FSTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FSTM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FSTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FSTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FSTM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FSTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FSTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FSTM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

             WHEN WRK-INP-PFK-FSTM         EQUAL 'Y' OR
                 (WRK-INP-PFK-FSTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FSTM     EQUAL 'PFK11')
                  PERFORM 2272-TRANSFERIR-CONTROLE

             WHEN WRK-INP-PFK-FSTM         EQUAL '7' OR
                 (WRK-INP-PFK-FSTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FSTM     EQUAL 'PFK07')
                  PERFORM 2250-TELA-ANTERIOR

             WHEN WRK-INP-PFK-FSTM         EQUAL '8' OR
                 (WRK-INP-PFK-FSTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FSTM     EQUAL 'PFK08')
                  PERFORM 2260-TELA-POSTERIOR

             WHEN
                 (WRK-INP-PFK-FSTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FSTM     EQUAL SPACES )
                  MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11> PARA
      -                'CONTINUAR '        TO WRK-OUT-MENSA-FSTM
                   PERFORM 2210-DEVOLVER-TELA-VARIA

             WHEN  OTHER
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FSTM
                   PERFORM 2210-DEVOLVER-TELA-VARIA

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-FSTM        TO WRK-OUT-SENHAS-FSTM
           MOVE WRK-INP-TIPOOPC-FSTM       TO WRK-OUT-TIPOOPC-FSTM
           MOVE WRK-INP-TIPOCON-FSTM       TO WRK-OUT-TIPOCON-FSTM
           MOVE WRK-INP-PASSO-FSTM         TO WRK-OUT-PASSO-FSTM
           MOVE WRK-INP-CHAMADO-FSTM       TO WRK-OUT-CHAMADO-FSTM
           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-OUT-TMSTAMP-FSTM
           MOVE WRK-INP-ULTPAG-FSTM        TO WRK-OUT-ULTPAG-FSTM
           MOVE WRK-INP-PAGINA-FSTM-X      TO WRK-OUT-PAGINA-FSTM-X
           MOVE WRK-INP-NCLIE-FSTM         TO WRK-OUT-NCLIE-FSTM
           MOVE WRK-INP-CCNPJ-FSTM         TO WRK-OUT-CCNPJ-FSTM
           MOVE WRK-INP-CAGCT-FSTM         TO WRK-OUT-CAGCT-FSTM
           MOVE WRK-INP-CPROD-FSTM-X       TO WRK-OUT-CPROD-FSTM-X
           MOVE WRK-INP-NPROD-FSTM         TO WRK-OUT-NPROD-FSTM
           MOVE WRK-INP-CSUBP-FSTM-X       TO WRK-OUT-CSUBP-FSTM-X
           MOVE WRK-INP-NSUBP-FSTM         TO WRK-OUT-NSUBP-FSTM
           MOVE WRK-INP-COPER-FSTM-X       TO WRK-OUT-COPER-FSTM-X
           MOVE WRK-INP-NSITU-FSTM         TO WRK-OUT-NSITU-FSTM
           MOVE WRK-INP-NSIT2-FSTM         TO WRK-OUT-NSIT2-FSTM
           MOVE WRK-INP-CSITU-FSTM-X       TO WRK-OUT-CSITU-FSTM-X
           MOVE WRK-INP-MENSA-FSTM         TO WRK-OUT-MENSA-FSTM.

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

             MOVE WRK-INP-CTPLB-FSTM(WRK-IND)      TO
                       WRK-OUT-CTPLB-FSTM(WRK-IND)
             MOVE WRK-INP-NTPLB-FSTM(WRK-IND)      TO
                       WRK-OUT-NTPLB-FSTM(WRK-IND)
             MOVE WRK-INP-FAVOR-FSTM(WRK-IND)      TO
                       WRK-OUT-FAVOR-FSTM(WRK-IND)
             MOVE WRK-INP-CAGCT-FSTM-TB(WRK-IND)   TO
                       WRK-OUT-CAGCT-FSTM-TB(WRK-IND)
             MOVE WRK-INP-CCNPJ-FSTM-TB(WRK-IND)   TO
                       WRK-OUT-CCNPJ-FSTM-TB(WRK-IND)
             MOVE WRK-INP-PERCE-FSTM(WRK-IND)      TO
                       WRK-OUT-PERCE-FSTM(WRK-IND)
             MOVE WRK-INP-VOPER-FSTM(WRK-IND)      TO
                       WRK-OUT-VOPER-FSTM(WRK-IND)
             MOVE WRK-INP-DATAO-FSTM(WRK-IND)      TO
                       WRK-OUT-DATAO-FSTM(WRK-IND)
           END-PERFORM.


      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0618                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-IMS
           MOVE SPACES                     TO COMU-DADOS

           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 2120-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)    TO WRK-TELA
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 2120-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA-IMS       TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0619'                 TO COMU-TRANSACAO
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

           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 2120-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0619'                 TO WRK-TELA
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

           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 2120-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0619'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2250-TELA-ANTERIOR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-FSTM         EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)        TO WRK-OUT-MENSA-FSTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE 'N'                        TO ENT5527-START-COUNT
           MOVE ZEROS                      TO ENT5527-FLAG
           MOVE ZEROS                      TO ENT5527-RESTART
           MOVE WRK-COD-USER               TO ENT5527-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5527-CTERM
           MOVE WRK-INP-COPER-FSTM(1:4)    TO ENT5527-DANO-OPER-DESC
           MOVE WRK-INP-COPER-FSTM(5:9)    TO ENT5527-NSEQ-OPER-DESC
           COMPUTE ENT5527-RESTART = (WRK-INP-PAGINA-FSTM - 2) * 4.

260309     MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP
260309     MOVE 1                          TO WRK-ENT-PAGINA
260309     MOVE 'CP'                       TO WRK-ENT-OPCAO
260309
260309     PERFORM 2120-ACESSAR-DCOM7995
260309
260309     MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL
260309     MOVE WRK-TIMESTAMP              TO ENT5527-HULT-ATULZ.

       2250-ACESSAR-DCOM5527.

           MOVE 'DCOM5527'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5527
                                              SAIDA-ROT5527
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5527-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
                IF SAI5527-CMEIO-LIBRC-DESC(5) EQUAL ZEROS
                   MOVE '*'                TO WRK-OUT-ULTPAG-FSTM
                   MOVE SG-MENSAGEM(17)
                                           TO WRK-OUT-MENSA-FSTM
                ELSE
                  MOVE SPACES              TO WRK-OUT-ULTPAG-FSTM
                  MOVE SG-MENSAGEM(01)     TO WRK-OUT-MENSA-FSTM
                END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 04

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN '0003'
                IF  ENT5527-OFIC-HIST      EQUAL 'H'
                    INITIALIZE SAIDA-ROT5527
                    MOVE 'O'               TO ENT5527-OFIC-HIST
                    GO TO 2250-ACESSAR-DCOM5527
                ELSE
                    MOVE SAI5527-MSG-RETORNO   TO WRK-OUT-MENSA-FSTM
                    GO TO 2250-99-FIM
                END-IF

             WHEN OTHER
                MOVE SAI5527-MSG-RETORNO   TO WRK-OUT-MENSA-FSTM
                PERFORM 2210-DEVOLVER-TELA-VARIA
                GO TO 2250-99-FIM

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FSTM = WRK-INP-PAGINA-FSTM - 1.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2260-TELA-POSTERIOR                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-FSTM         EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)        TO WRK-OUT-MENSA-FSTM
               GO TO 2260-99-FIM
           END-IF.

           MOVE 'N'                        TO ENT5527-START-COUNT
           MOVE ZEROS                      TO ENT5527-FLAG
           MOVE ZEROS                      TO ENT5527-RESTART
           MOVE WRK-COD-USER               TO ENT5527-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5527-CTERM
           MOVE WRK-INP-COPER-FSTM(1:4)    TO ENT5527-DANO-OPER-DESC
           MOVE WRK-INP-COPER-FSTM(5:9)    TO ENT5527-NSEQ-OPER-DESC
           COMPUTE ENT5527-RESTART = (WRK-INP-PAGINA-FSTM) * 4 .

260309     MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP
260309     MOVE 1                          TO WRK-ENT-PAGINA
260309     MOVE 'CP'                       TO WRK-ENT-OPCAO
260309
260309     PERFORM 2120-ACESSAR-DCOM7995
260309
260309     MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL
260309     MOVE WRK-TIMESTAMP              TO ENT5527-HULT-ATULZ.

       2260-ACESSAR-DCOM5527.

           MOVE 'DCOM5527'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5527
                                              SAIDA-ROT5527
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5527-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
               IF SAI5527-CMEIO-LIBRC-DESC(5) EQUAL ZEROS
                   MOVE '*'                TO WRK-OUT-ULTPAG-FSTM
                   MOVE SG-MENSAGEM(17)    TO WRK-OUT-MENSA-FSTM
               ELSE
                   MOVE SPACES             TO WRK-OUT-ULTPAG-FSTM
                   MOVE SG-MENSAGEM(01)    TO WRK-OUT-MENSA-FSTM
               END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 04

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN '0003'
                IF  ENT5527-OFIC-HIST      EQUAL 'H'
                    INITIALIZE SAIDA-ROT5527
                    MOVE 'O'               TO ENT5527-OFIC-HIST
                    GO TO 2260-ACESSAR-DCOM5527
                ELSE
                    MOVE SAI5527-MSG-RETORNO   TO WRK-OUT-MENSA-FSTM
                END-IF

             WHEN OTHER
                MOVE SAI5527-MSG-RETORNO   TO WRK-OUT-MENSA-FSTM
                PERFORM 2210-DEVOLVER-TELA-VARIA
                GO TO 2260-99-FIM

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FSTM = WRK-INP-PAGINA-FSTM + 1.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FSTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 2120-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0619'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS
           PERFORM 2120-ACESSAR-DCOM7995
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL.
           MOVE 'DCOM0619'                 TO COMU-TRANSACAO
           MOVE 'DCOM0620'                 TO WRK-TELA
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FSTM
                                       TO  WRK-660-LL-MENS-FSTM
                                           WRK-OUT-LL-FSTM

           MOVE LENGTH OF WRK-660-DCOMFSTM
                                       TO  WRK-660-LL-AREA-FSTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FSTM
                                              WRK-660-DCOMFSTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FSTM            TO WRK-MENSAGEM.

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
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0619'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0619'             TO ERR-MODULO
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
