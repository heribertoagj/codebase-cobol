      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0613.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0613                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: NELSON TANAKA - ALTRAN.                      *
      *     DATA........: 21/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA CONSULTA OPERACAO - MEIO DE LIBERACAO  *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFMTM....: LISTA CONSULTA OPERACAO - MEIO DE LIBERACAO  *
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
      *     CHAMADO POR.: DCOM0612                  CHAMA: DCOM0614    *
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
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0613 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
       77  WRK-VOPER-E                     PIC Z.ZZZ.ZZZ.ZZ9,99.
       77  WRK-PERC-E                      PIC ZZ9,99.
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

       COPY 'I#DCOMJ2'.
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
           '*** AREA DA TELA DCOMFMTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFMTM.
           05  WRK-660-LL-AREA-FMTM        PIC  9(04) COMP VALUE 0112.
           05  WRK-660-LL-MENS-FMTM        PIC  9(04) COMP VALUE 1024.
           05  WRK-660-SENHAS-FMTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FMTM        PIC  9(04) COMP VALUE 0068.
           05  WRK-660-TMSTAMP-FMTM        PIC  9(04) COMP VALUE 0026.
           05  WRK-660-TIPOOPC-FMTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-TIPOCON-FMTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-ULTPAG-FMTM         PIC  9(04) COMP VALUE 0001.
           05  WRK-660-PASSO-FMTM          PIC  9(04) COMP VALUE 0001.
           05  WRK-660-OCULTO-FMTM         PIC  9(04) COMP VALUE 0008.
           05  WRK-660-PAGINA-FMTM         PIC  9(04) COMP VALUE 0004.
           05  WRK-660-NCLIE-FMTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CCNPJ-FMTM          PIC  9(04) COMP VALUE 0019.
           05  WRK-660-CAGCT-FMTM          PIC  9(04) COMP VALUE 0023.
           05  WRK-660-CPROD-FMTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NPROD-FMTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-CSUBP-FMTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NSUBP-FMTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-COPER-FMTM          PIC  9(04) COMP VALUE 0013.
           05  WRK-660-NSITU-FMTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-NSIT2-FMTM          PIC  9(04) COMP VALUE 0009.
           05  WRK-660-CSITU-FMTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-600-LISTA               OCCURS  04.
               10 WRK-660-CTPLB-FMTM       PIC  9(04) COMP VALUE 0003.
               10 WRK-660-NTPLB-FMTM       PIC  9(04) COMP VALUE 0015.
               10 WRK-660-FAVOR-FMTM       PIC  9(04) COMP VALUE 0050.
               10 WRK-660-CAGCT-FMTM       PIC  9(04) COMP VALUE 0023.
               10 WRK-660-CCNPJ-FMTM       PIC  9(04) COMP VALUE 0017.
               10 WRK-660-PERCE-FMTM       PIC  9(04) COMP VALUE 0006.
               10 WRK-660-VOPER-FMTM       PIC  9(04) COMP VALUE 0016.
               10 WRK-660-DATAO-FMTM       PIC  9(04) COMP VALUE 0010.
           05  WRK-660-SALVA-FMTM          PIC  9(04) COMP VALUE 0079.
           05  WRK-660-MENSA-FMTM          PIC  9(04) COMP VALUE 0079.
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
           '*** AREA DA TELA DCOMFMTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FMTM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FMTM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FMTM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FMTM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FMTM.
               10  WRK-INP-TMSTAMP-FMTM    PIC  X(26)      VALUE SPACES.
               10  WRK-INP-TIPOOPC-FMTM    PIC  X(01)      VALUE SPACES.
               10  WRK-INP-TIPOCON-FMTM    PIC  X(01)      VALUE SPACES.
               10  WRK-INP-ULTPAG-FMTM     PIC  X(01)      VALUE SPACE.
               10  WRK-INP-PASSO-FMTM      PIC  X(01)      VALUE SPACE.
               10  WRK-INP-OCULTO-FMTM.
                   15  WRK-INP-TPPESQ-FMTM PIC  X(01)      VALUE SPACE.
                   15  FILLER              PIC  X(07)      VALUE SPACE.
               10  WRK-INP-PAGINA-FMTM-X.
                   15  WRK-INP-PAGINA-FMTM
                                           PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-INP-NCLIE-FMTM      PIC  X(40)      VALUE SPACE.
               10  WRK-INP-CCNPJ-FMTM      PIC  X(19)      VALUE SPACE.
               10  WRK-INP-CAGCT-FMTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-INP-CPROD-FMTM-X.
                   15  WRK-INP-CPROD-FMTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NPROD-FMTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-INP-CSUBP-FMTM-X.
                   15  WRK-INP-CSUBP-FMTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NSUBP-FMTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-INP-COPER-FMTM-X.
                   15  WRK-INP-COPER-FMTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-INP-NSITU-FMTM      PIC  X(15)      VALUE SPACE.
               10  WRK-INP-NSIT2-FMTM      PIC  X(09)      VALUE SPACES.
               10  WRK-INP-CSITU-FMTM-X.
                   15  WRK-INP-CSITU-FMTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-INP-LINHAS-FMTM OCCURS 04 TIMES.
                   15  WRK-INP-CTPLB-FMTM  PIC  X(03)     VALUE SPACES.
                   15  WRK-INP-NTPLB-FMTM  PIC  X(15)     VALUE SPACES.
                   15  WRK-INP-FAVOR-FMTM  PIC  X(50)     VALUE SPACES.
                   15  WRK-INP-CAGCT-FMTM-TB
                                           PIC  X(23)     VALUE SPACES.
                   15  WRK-INP-CCNPJ-FMTM-TB
                                           PIC  X(17)     VALUE SPACES.
                   15  WRK-INP-PERCE-FMTM  PIC  X(06)     VALUE SPACES.
                   15  WRK-INP-VOPER-FMTM  PIC  X(16)     VALUE SPACES.
                   15  WRK-INP-DATAO-FMTM  PIC  X(10)     VALUE SPACES.
           05  WRK-INP-SALVA-FMTM          PIC  X(79)     VALUE SPACES.
           05  WRK-INP-MENSA-FMTM          PIC  X(79)     VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFMTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FMTM.
           05  WRK-OUT-LL-FMTM             PIC S9(04) COMP VALUE +1024.
           05  WRK-OUT-ZZ-FMTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FMTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-FMTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FMTM.
               10  WRK-OUT-TMSTAMP-FMTM    PIC  X(26)      VALUE SPACES.
               10  WRK-OUT-TIPOOPC-FMTM    PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-TIPOCON-FMTM    PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-ULTPAG-FMTM     PIC  X(01)      VALUE SPACE.
               10  WRK-OUT-PASSO-FMTM      PIC  X(01)      VALUE SPACE.
               10  WRK-OUT-OCULTO-FMTM.
                   15  WRK-OUT-TPPESQ-FMTM PIC  X(01)      VALUE SPACE.
                   15  FILLER              PIC  X(07)      VALUE SPACE.
               10  WRK-OUT-PAGINA-FMTM-X.
                   15  WRK-OUT-PAGINA-FMTM PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-OUT-NCLIE-FMTM      PIC  X(40)      VALUE SPACE.
               10  WRK-OUT-CCNPJ-FMTM      PIC  X(19)      VALUE SPACE.
               10  WRK-OUT-CAGCT-FMTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-OUT-CPROD-FMTM-X.
                   15  WRK-OUT-CPROD-FMTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NPROD-FMTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-OUT-CSUBP-FMTM-X.
                   15  WRK-OUT-CSUBP-FMTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NSUBP-FMTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-OUT-COPER-FMTM-X.
                   15  WRK-OUT-COPER-FMTM  PIC  9(13)      VALUE ZEROS.
      *
               10  WRK-OUT-NSITU-FMTM      PIC  X(15)      VALUE SPACE.
               10  WRK-OUT-NSIT2-FMTM      PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-CSITU-FMTM-X.
                   15  WRK-OUT-CSITU-FMTM  PIC  9(10)      VALUE ZEROS.
      *
               10  WRK-OUT-LINHAS-FMTM OCCURS 04 TIMES.
                   15  WRK-OUT-CTPLB-FMTM  PIC  X(03)      VALUE SPACES.
                   15  WRK-OUT-NTPLB-FMTM  PIC  X(15)      VALUE SPACES.
                   15  WRK-OUT-FAVOR-FMTM  PIC  X(50)      VALUE SPACES.
                   15  WRK-OUT-CAGCT-FMTM-TB
                                           PIC  X(23)      VALUE SPACES.
                   15  WRK-OUT-CCNPJ-FMTM-TB
                                           PIC  X(17)      VALUE SPACES.
                   15  WRK-OUT-PERCE-FMTM  PIC  X(06)      VALUE SPACES.
                   15  WRK-OUT-VOPER-FMTM  PIC  X(16)      VALUE SPACES.
                   15  WRK-OUT-DATAO-FMTM  PIC  X(10)      VALUE SPACES.
           05  WRK-OUT-SALVA-FMTM          PIC  X(79)      VALUE SPACES.
           05  WRK-OUT-MENSA-FMTM          PIC  X(79)      VALUE SPACES.
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
           '*** DCOM0613 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FMTM
                                              WRK-OUT-SENHAS-FMTM
                                              WRK-OUT-COMANDO-FMTM
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

           MOVE 'DCOMFMTM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0613'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFMTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFMTM'
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
           MOVE COMU-TIMESTAMP             TO WRK-INP-TMSTAMP-FMTM
                                              WRK-OUT-TMSTAMP-FMTM
                                              WRK-ENT-TIMESTAMP.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-DANO-OPER-DESC   TO WRK-OUT-COPER-FMTM-X(1:4).
           MOVE WRK-NSEQ-OPER-DESC   TO WRK-OUT-COPER-FMTM-X(5:9).
           MOVE WRK-NOMECLI          TO WRK-OUT-NCLIE-FMTM.
           MOVE WRK-CNPJCPF          TO WRK-OUT-CCNPJ-FMTM.
           STRING  '237/' WRK-AGENCIA '/'
                   WRK-CONTA         DELIMITED BY SIZE
                                     INTO WRK-OUT-CAGCT-FMTM

           MOVE WRK-SITU1                  TO WRK-OUT-NSITU-FMTM.
           MOVE WRK-SITU2                  TO WRK-OUT-NSIT2-FMTM.
           MOVE WRK-SITU3                  TO WRK-OUT-CSITU-FMTM-X.


           MOVE WRK-PRODUTO                TO WRK-OUT-CPROD-FMTM.
           MOVE WRK-DESC-PRODUTO           TO WRK-OUT-NPROD-FMTM.
           MOVE WRK-SUBPRODUTO             TO WRK-OUT-CSUBP-FMTM.
           MOVE WRK-DESC-SPROD             TO WRK-OUT-NSUBP-FMTM.

           INITIALIZE  ENTRADA-ROT5527
                       SAIDA-ROT5527

           MOVE 'O'                        TO ENT5527-OFIC-HIST
                                              WRK-OUT-TPPESQ-FMTM
                                              WRK-INP-TPPESQ-FMTM

           PERFORM 7000-ACESSAR-DCOM5527.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0613'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE 1                          TO WRK-OUT-PAGINA-FMTM.

           IF  COMU-MENSAGEM               EQUAL SPACES
               MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11> P/ CONT
      -             'INUAR OU <PF3> P/ RETORNAR'  TO WRK-OUT-MENSA-FMTM
           ELSE
               MOVE COMU-MENSAGEM          TO WRK-OUT-MENSA-FMTM
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
       7000-ACESSAR-DCOM5527               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                        TO ENT5527-START-COUNT
           MOVE ZEROS                      TO ENT5527-FLAG
           MOVE ZEROS                      TO ENT5527-RESTART
           MOVE WRK-COD-USER               TO ENT5527-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5527-CTERM
           MOVE WRK-DANO-OPER-DESC         TO ENT5527-DANO-OPER-DESC
           MOVE WRK-NSEQ-OPER-DESC         TO ENT5527-NSEQ-OPER-DESC
           MOVE 'DCOM5527'                 TO WRK-MODULO .

           CALL WRK-MODULO             USING  ENTRADA-ROT5527
                                              SAIDA-ROT5527
                                              ERRO-AREA
                                              WRK-SQLCA .

           EVALUATE SAI5527-COD-RETORNO
              WHEN '0000'
              WHEN '0002'
                IF  SAI5527-CMEIO-LIBRC-DESC(5) EQUAL ZEROS
                    MOVE '*'               TO WRK-OUT-ULTPAG-FMTM
                    MOVE SG-MENSAGEM(63)   TO WRK-OUT-MENSA-FMTM
                ELSE
                    MOVE SPACES            TO WRK-OUT-ULTPAG-FMTM
                    MOVE SPACES            TO WRK-OUT-MENSA-FMTM
                END-IF
                PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 04

              WHEN '0003'
                IF  ENT5527-OFIC-HIST      EQUAL 'O'
                    INITIALIZE SAIDA-ROT5527
                    MOVE 'T'               TO ENT5527-OFIC-HIST
                                              WRK-OUT-TPPESQ-FMTM
                                              WRK-INP-TPPESQ-FMTM
                    GO TO 7000-ACESSAR-DCOM5527
                END-IF

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 04

           END-EVALUATE.

      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
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
                                         TO WRK-OUT-CTPLB-FMTM(WRK-IND)
               MOVE SAI5527-IRSUMO-MEIO-LIBRC(WRK-IND)
                                         TO WRK-OUT-NTPLB-FMTM(WRK-IND)
               MOVE SAI5527-IPSSOA-DESC-COML(WRK-IND)
                                         TO WRK-OUT-FAVOR-FMTM(WRK-IND)
               MOVE SAI5527-CBCO(WRK-IND)
                                         TO WRK-CBCO
               MOVE SAI5527-CAG-BCRIA(WRK-IND)
                                         TO WRK-CAG-BCRIA
               MOVE SAI5527-CCTA-BCRIA-CLI(WRK-IND)
                                         TO WRK-CCTA-BCRIA-CLI
               MOVE WRK-BCO-AGE-CTA      TO
                                         WRK-OUT-CAGCT-FMTM-TB(WRK-IND)
               MOVE SAI5527-CCNPJ-CPF(WRK-IND)
                                         TO WRK-CCNPJ-CPF-E
               MOVE SAI5527-CFLIAL-CNPJ(WRK-IND)
                                         TO WRK-CFLIAL-CNPJ-E
               MOVE SAI5527-CCTRL-CNPJ-CPF(WRK-IND)
                                         TO WRK-CCTRL-CNPJ-CPF-E
               MOVE WRK-CNPJ-E           TO
                                         WRK-OUT-CCNPJ-FMTM-TB(WRK-IND)
               MOVE SAI5527-PLIBRC-DESC-COML(WRK-IND)
                                         TO WRK-PERC-E
               MOVE WRK-PERC-E           TO WRK-OUT-PERCE-FMTM(WRK-IND)
               MOVE SAI5527-VLIBRC-DESC-COML(WRK-IND)
                                         TO WRK-VOPER-E
               MOVE WRK-VOPER-E          TO WRK-OUT-VOPER-FMTM(WRK-IND)
               MOVE SAI5527-DLIBRC-DESC-COML(WRK-IND)
                                         TO WRK-OUT-DATAO-FMTM(WRK-IND)
               INSPECT WRK-OUT-DATAO-FMTM(WRK-IND) REPLACING ALL '.'
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

           MOVE SPACES                TO WRK-OUT-CTPLB-FMTM(WRK-IND)
                                         WRK-OUT-NTPLB-FMTM(WRK-IND)
                                         WRK-OUT-FAVOR-FMTM(WRK-IND)
                                         WRK-OUT-CAGCT-FMTM-TB(WRK-IND)
                                         WRK-OUT-CCNPJ-FMTM-TB(WRK-IND)
                                         WRK-OUT-PERCE-FMTM(WRK-IND)
                                         WRK-OUT-VOPER-FMTM(WRK-IND)
                                         WRK-OUT-DATAO-FMTM(WRK-IND).

      *----------------------------------------------------------------*
       2115-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFMTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFMTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FMTM

           PERFORM 2205-DEVOLVER-TELA-FIXO

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FMTM         EQUAL 'H' OR
                  (WRK-INP-PFK-FMTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FMTM    EQUAL 'PFK01')
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-INP-MENSA-FMTM TO  WRK-OUT-MENSA-FMTM

             WHEN WRK-INP-PFK-FMTM         EQUAL '3' OR
                   (WRK-INP-PFK-FMTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FMTM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FMTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FMTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FMTM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FMTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FMTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FMTM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

             WHEN WRK-INP-PFK-FMTM         EQUAL 'Y' OR
                 (WRK-INP-PFK-FMTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FMTM     EQUAL 'PFK11')
                  PERFORM 2272-TRANSFERIR-CONTROLE

             WHEN WRK-INP-PFK-FMTM         EQUAL '7' OR
                 (WRK-INP-PFK-FMTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FMTM     EQUAL 'PFK07')
                  PERFORM 2250-TELA-ANTERIOR

             WHEN WRK-INP-PFK-FMTM         EQUAL '8' OR
                 (WRK-INP-PFK-FMTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FMTM     EQUAL 'PFK08')
                  PERFORM 2260-TELA-POSTERIOR

             WHEN
                 (WRK-INP-PFK-FMTM         EQUAL '.' AND
                  WRK-INP-COMANDO-FMTM     EQUAL SPACES )
               MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11> P/ CONT
      -             'INUAR OU <PF3> P/ RETORNAR'  TO WRK-OUT-MENSA-FMTM
                   PERFORM 2210-DEVOLVER-TELA-VARIA

             WHEN  OTHER
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FMTM
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

           MOVE WRK-INP-TIPOOPC-FMTM       TO WRK-OUT-TIPOOPC-FMTM
           MOVE WRK-INP-TIPOCON-FMTM       TO WRK-OUT-TIPOCON-FMTM
           MOVE WRK-INP-PASSO-FMTM         TO WRK-OUT-PASSO-FMTM
           MOVE WRK-INP-OCULTO-FMTM        TO WRK-OUT-OCULTO-FMTM
           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-OUT-TMSTAMP-FMTM
           MOVE WRK-INP-ULTPAG-FMTM        TO WRK-OUT-ULTPAG-FMTM
           MOVE WRK-INP-PAGINA-FMTM-X      TO WRK-OUT-PAGINA-FMTM-X
           MOVE WRK-INP-NCLIE-FMTM         TO WRK-OUT-NCLIE-FMTM
           MOVE WRK-INP-CCNPJ-FMTM         TO WRK-OUT-CCNPJ-FMTM
           MOVE WRK-INP-CAGCT-FMTM         TO WRK-OUT-CAGCT-FMTM
           MOVE WRK-INP-CPROD-FMTM-X       TO WRK-OUT-CPROD-FMTM-X
           MOVE WRK-INP-NPROD-FMTM         TO WRK-OUT-NPROD-FMTM
           MOVE WRK-INP-CSUBP-FMTM-X       TO WRK-OUT-CSUBP-FMTM-X
           MOVE WRK-INP-NSUBP-FMTM         TO WRK-OUT-NSUBP-FMTM
           MOVE WRK-INP-COPER-FMTM-X       TO WRK-OUT-COPER-FMTM-X
           MOVE WRK-INP-NSITU-FMTM         TO WRK-OUT-NSITU-FMTM
           MOVE WRK-INP-NSIT2-FMTM         TO WRK-OUT-NSIT2-FMTM
           MOVE WRK-INP-CSITU-FMTM-X       TO WRK-OUT-CSITU-FMTM-X
           MOVE WRK-INP-MENSA-FMTM         TO WRK-OUT-MENSA-FMTM.

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

             MOVE WRK-INP-CTPLB-FMTM(WRK-IND)      TO
                       WRK-OUT-CTPLB-FMTM(WRK-IND)
             MOVE WRK-INP-NTPLB-FMTM(WRK-IND)      TO
                       WRK-OUT-NTPLB-FMTM(WRK-IND)
             MOVE WRK-INP-FAVOR-FMTM(WRK-IND)      TO
                       WRK-OUT-FAVOR-FMTM(WRK-IND)
             MOVE WRK-INP-CAGCT-FMTM-TB(WRK-IND)   TO
                       WRK-OUT-CAGCT-FMTM-TB(WRK-IND)
             MOVE WRK-INP-CCNPJ-FMTM-TB(WRK-IND)   TO
                       WRK-OUT-CCNPJ-FMTM-TB(WRK-IND)
             MOVE WRK-INP-PERCE-FMTM(WRK-IND)      TO
                       WRK-OUT-PERCE-FMTM(WRK-IND)
             MOVE WRK-INP-VOPER-FMTM(WRK-IND)      TO
                       WRK-OUT-VOPER-FMTM(WRK-IND)
             MOVE WRK-INP-DATAO-FMTM(WRK-IND)      TO
                       WRK-OUT-DATAO-FMTM(WRK-IND)
           END-PERFORM.


      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0612                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-IMS
           MOVE SPACES                     TO COMU-DADOS

           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               MOVE WRK-CHAMADOR(IND-1)     TO WRK-TELA
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE LENGTH COMU-AREA-IMS       TO COMU-LL
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0613'                 TO COMU-TRANSACAO
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

           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0613'                 TO WRK-TELA
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

           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0613'                 TO WRK-TELA
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

           IF  WRK-INP-PAGINA-FMTM         EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)        TO WRK-OUT-MENSA-FMTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE 'N'                        TO ENT5527-START-COUNT
           MOVE ZEROS                      TO ENT5527-FLAG
           MOVE ZEROS                      TO ENT5527-RESTART
           MOVE WRK-COD-USER               TO ENT5527-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5527-CTERM
           MOVE WRK-INP-COPER-FMTM(1:4)    TO ENT5527-DANO-OPER-DESC
           MOVE WRK-INP-COPER-FMTM(5:9)    TO ENT5527-NSEQ-OPER-DESC
           COMPUTE ENT5527-RESTART = (WRK-INP-PAGINA-FMTM - 2) * 4
           MOVE WRK-INP-TPPESQ-FMTM        TO ENT5527-OFIC-HIST.

           MOVE 'DCOM5527'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5527
                                              SAIDA-ROT5527
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5527-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
                IF SAI5527-CMEIO-LIBRC-DESC(5) EQUAL ZEROS
                   MOVE '*'                TO WRK-OUT-ULTPAG-FMTM
                   MOVE SG-MENSAGEM(17)
                                           TO WRK-OUT-MENSA-FMTM
                ELSE
                  MOVE SPACES              TO WRK-OUT-ULTPAG-FMTM
                  MOVE SG-MENSAGEM(01)     TO WRK-OUT-MENSA-FMTM
                END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 04

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FMTM = WRK-INP-PAGINA-FMTM - 1.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2260-TELA-POSTERIOR                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-FMTM         EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)        TO WRK-OUT-MENSA-FMTM
               GO TO 2260-99-FIM
           END-IF.

           MOVE 'N'                        TO ENT5527-START-COUNT
           MOVE ZEROS                      TO ENT5527-FLAG
           MOVE ZEROS                      TO ENT5527-RESTART
           MOVE WRK-COD-USER               TO ENT5527-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO ENT5527-CTERM
           MOVE WRK-INP-COPER-FMTM(1:4)    TO ENT5527-DANO-OPER-DESC
           MOVE WRK-INP-COPER-FMTM(5:9)    TO ENT5527-NSEQ-OPER-DESC
           COMPUTE ENT5527-RESTART = (WRK-INP-PAGINA-FMTM) * 4
           MOVE WRK-INP-TPPESQ-FMTM        TO ENT5527-OFIC-HIST.

           MOVE 'DCOM5527'                 TO WRK-MODULO
           CALL WRK-MODULO             USING  ENTRADA-ROT5527
                                              SAIDA-ROT5527
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE SAI5527-COD-RETORNO
             WHEN '0000'
             WHEN '0002'
               IF SAI5527-CMEIO-LIBRC-DESC(5) EQUAL ZEROS
                   MOVE '*'                TO WRK-OUT-ULTPAG-FMTM
                   MOVE SG-MENSAGEM(17)    TO WRK-OUT-MENSA-FMTM
               ELSE
                   MOVE SPACES             TO WRK-OUT-ULTPAG-FMTM
                   MOVE SG-MENSAGEM(01)    TO WRK-OUT-MENSA-FMTM
               END-IF

               PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                          UNTIL WRK-IND GREATER 04

             WHEN '0099'
               MOVE 'DB2'                  TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN OTHER
                PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.

           COMPUTE WRK-OUT-PAGINA-FMTM = WRK-INP-PAGINA-FMTM + 1.

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
           MOVE WRK-INP-TMSTAMP-FMTM       TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0613'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL.
           MOVE 'DCOM0613'                 TO COMU-TRANSACAO
           MOVE 'DCOM0614'                 TO WRK-TELA
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FMTM
                                       TO  WRK-660-LL-MENS-FMTM
                                           WRK-OUT-LL-FMTM

           MOVE LENGTH OF WRK-660-DCOMFMTM
                                       TO  WRK-660-LL-AREA-FMTM

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FMTM
                                              WRK-660-DCOMFMTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FMTM            TO WRK-MENSAGEM.

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
               MOVE 'DCOM0613'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0613'             TO ERR-MODULO
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
