      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM0617.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0617                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: EMERSON ARTUR FRANCO - ALTRAN.               *
      *     DATA........: 21/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: CONSULTA OPERACAO - DEMONSTRATIVO            *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMFQTM....: CONSULTA OPERACAO - DEMONSTRATIVO            *
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
      *     DCOM4310 - OBTER OPERACOES POR AGENCIA                     *
      *     DCOM5537 - MODULO DE ACESSO                                *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.:                            CHAMA: DCOM0618   *
      *================================================================*
BSI001*                    ALTERACAO  -  BSI TECNOLOGIA                *
BSI001*----------------------------------------------------------------*
BSI001*    ANALISTA BSI:   LUCIANDRA SILVEIRA.                         *
BSI001*    ANALISTA DDS:   RICARDO JAMMAL.                             *
BSI001*    DATA........:   07/08/2013                                  *
BSI001*----------------------------------------------------------------*
BSI001*    OBJETIVO....:   NAO VERIFICAR PERFIL PARA A FUNCIONALIDADE  *
BSI001*                    DE CONSULTA NP-RURAL.                       *
BSI001*================================================================*
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
           '*** DCOM0617 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WRK-IND                         PIC S9(09) COMP VALUE ZEROS.
       77  IND-1                           PIC S9(09) COMP VALUE ZEROS.
       77  WIND-SEL                        PIC S9(09) COMP VALUE ZEROS.
       77  WCT-SELEC                       PIC S9(09) COMP VALUE ZEROS.
      *
      *---------------------------------------------------------------*
       77  FILLER                          PIC  X(50)      VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                         PIC S9(08) COMP VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-241                         PIC S9(08) COMP VALUE +241.
      *--- ALPHA    - NORMAL    - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49345                       PIC S9(08) COMP VALUE +49345.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       77  WRK-49361                       PIC S9(08) COMP VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       77  WRK-49369                       PIC S9(08) COMP VALUE +49369.
      *
      *----------------------------------------------------------------*
       77  FILLER                          PIC  X(50)      VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO                  PIC  X(08)      VALUE SPACES.
           05  WRK-INCONSIS                PIC  X(01)      VALUE SPACES.
           05  WRK-RESTART                 PIC  9(05)      VALUE ZEROS.
      *
           05  WRK-CNPJ-E.
FS2511*        10 WRK-CCNPJ-CPF-E          PIC 9(09)       VALUE ZEROS.
FS2511         10 WRK-CCNPJ-CPF-E          PIC X(09)       VALUE SPACES.
               10 FILLER                   PIC X(01)       VALUE '/'.
FS2511*        10 WRK-CFQIAL-CNPJ-E        PIC 9(04)       VALUE ZEROS.
FS2511         10 WRK-CFQIAL-CNPJ-E        PIC X(04)       VALUE SPACES.
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
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMAS CHAMADORES ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-IMS.
           05  COMU-LL-FQTM              PIC S9(004) COMP  VALUE +406.
           05  COMU-ZZ-FQTM              PIC S9(004) COMP  VALUE ZEROS.
           05  COMU-TRANCODE-FQTM.
               10  COMU-TRANSACAO-FQTM   PIC  X(008)       VALUE SPACES.
               10  FILLER                PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-FQTM         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-FQTM          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-FQTM         PIC  X(068)       VALUE SPACES.
           05  COMU-TIMESTAMP-FQTM       PIC  X(026)       VALUE SPACES.
           05  COMU-MENSAGEM-FQTM        PIC  X(079)       VALUE SPACES.
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
           05  WRK-COD-DEPTO.
               10  FILLER                 PIC  X(02)      VALUE SPACES.
               10  WRK-COD-DEPTO-N        PIC  9(04)      VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER                  PIC  X(04)      VALUE SPACES.
               10  WRK-TRANSACAO           PIC  X(08)      VALUE SPACES.
               10  FILLER                  PIC  X(1988)    VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFQTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMFQTM.
           05  WRK-660-LL-AREA-FQTM        PIC  9(04) COMP VALUE 0000.
           05  WRK-660-LL-MENS-FQTM        PIC  9(04) COMP VALUE 0000.
           05  WRK-660-SENHAS-FQTM         PIC  9(04) COMP VALUE 0037.
           05  WRK-660-COMANDO-FQTM        PIC  9(04) COMP VALUE 0068.
           05  WRK-660-TIMEST-FQTM         PIC  9(04) COMP VALUE 0026.
           05  WRK-660-TPOPCAO-FQTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-TIPOCON-FQTM        PIC  9(04) COMP VALUE 0001.
           05  WRK-660-ULTPAG-FQTM         PIC  9(04) COMP VALUE 0001.
           05  WRK-660-PASSO-FQTM          PIC  9(04) COMP VALUE 0001.
           05  WRK-660-CHAMADO-FQTM        PIC  9(04) COMP VALUE 0008.
           05  WRK-660-PAGINA-FQTM         PIC  9(04) COMP VALUE 0004.
           05  WRK-660-NCLIE-FQTM          PIC  9(04) COMP VALUE 0040.
           05  WRK-660-CCNPJ-FQTM          PIC  9(04) COMP VALUE 0017.
           05  WRK-660-CAGCT-FQTM          PIC  9(04) COMP VALUE 0023.
           05  WRK-660-CPROD-FQTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NPROD-FQTM          PIC  9(04) COMP VALUE 0012.
           05  WRK-660-CSUBP-FQTM          PIC  9(04) COMP VALUE 0003.
           05  WRK-660-NSUBP-FQTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-COPER-FQTM          PIC  9(04) COMP VALUE 0013.
           05  WRK-660-NSITU-FQTM          PIC  9(04) COMP VALUE 0015.
           05  WRK-660-NSIT2-FQTM          PIC  9(04) COMP VALUE 0009.
           05  WRK-660-CSITU-FQTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-DTINC-FQTM          PIC  9(04) COMP VALUE 0010.
           05  WRK-660-VOPER-FQTM          PIC  9(04) COMP VALUE 0017.
           05  WRK-600-LISTA               OCCURS  08.
               10 WRK-660-SEL-FQTM         PIC  9(04) COMP VALUE 2003.
               10 WRK-660-DTHIS-FQTM       PIC  9(04) COMP VALUE 0010.
               10 WRK-660-DTADT-FQTM       PIC  9(04) COMP VALUE 0010.
               10 WRK-660-TIMES-FQTM       PIC  9(04) COMP VALUE 0026.
           05  WRK-660-MENSA-FQTM          PIC  9(04) COMP VALUE 0079.
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
           '*** AREA DA TELA DCOMFQTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-FQTM.
           05  FILLER                      PIC  X(18)      VALUE SPACES.
           05  WRK-INP-PFK-FQTM            PIC  X(01)      VALUE SPACES.
           05  WRK-INP-SENHAS-FQTM         PIC  X(37)      VALUE SPACES.
           05  WRK-INP-COMANDO-FQTM        PIC  X(68)      VALUE SPACES.
           05  WRK-INP-DADOS-FQTM.
               10  WRK-INP-TIMEST-FQTM     PIC  X(26)      VALUE SPACES.
               10  WRK-INP-TPOPCAO-FQTM    PIC  X(01)      VALUE SPACES.
               10  WRK-INP-TIPOCON-FQTM    PIC  X(01)      VALUE SPACES.
               10  WRK-INP-ULTPAG-FQTM     PIC  X(01)      VALUE SPACE.
               10  WRK-INP-PASSO-FQTM      PIC  X(01)      VALUE SPACE.
               10  WRK-INP-CHAMADO-FQTM    PIC  X(08)      VALUE SPACE.
               10  WRK-INP-PAGINA-FQTM-X.
                   15  WRK-INP-PAGINA-FQTM
                                           PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-INP-NCLIE-FQTM      PIC  X(40)      VALUE SPACE.
               10  WRK-INP-CCNPJ-FQTM      PIC  X(17)      VALUE SPACE.
               10  WRK-INP-CAGCT-FQTM      PIC  X(23)      VALUE SPACE.
      *
               10  WRK-INP-CPROD-FQTM-X.
                   15  WRK-INP-CPROD-FQTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NPROD-FQTM      PIC  X(12)      VALUE SPACE.
      *
               10  WRK-INP-CSUBP-FQTM-X.
                   15  WRK-INP-CSUBP-FQTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-INP-NSUBP-FQTM      PIC  X(10)      VALUE SPACE.
      *
               10  WRK-INP-COPER-FQTM-X.
                   15  WRK-INP-COPER-FQTM  PIC  9(13)      VALUE ZEROS.
                   15  FILLER REDEFINES WRK-INP-COPER-FQTM.
                       20  WRK-INP-DANO-OPER-FQTM     PIC  9(04).
                       20  WRK-INP-NSEQ-OPER-FQTM     PIC  9(09).
      *
               10  WRK-INP-NSITU-FQTM      PIC  X(15)      VALUE SPACE.
               10  WRK-INP-NSIT2-FQTM      PIC  X(09)      VALUE SPACES.
               10  WRK-INP-CSITU-FQTM-X.
                   15  WRK-INP-CSITU-FQTM  PIC  9(10)      VALUE ZEROS.
               10  WRK-INP-DTINC-FQTM      PIC  X(10)      VALUE SPACES.
               10  WRK-INP-VOPER-FQTM.
                   15  WRK-INP-VOPER-FQTM-E     PIC ZZ.ZZZ.ZZZ.ZZ9,99.
      *
               10  WRK-INP-LINHAS-FQTM OCCURS 08 TIMES.
                   15  WRK-INP-SELEC-FQTM
                                           PIC  X(01)      VALUE SPACES.
                   15  WRK-INP-DTHIS-FQTM  PIC  X(10)      VALUE SPACES.
                   15  WRK-INP-DTADT-FQTM  PIC  X(10)      VALUE SPACES.
                   15  WRK-INP-TIMES-FQTM  PIC  X(26)      VALUE SPACES.
           05  WRK-INP-MENSA-FQTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMFQTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-FQTM.
           05  WRK-OUT-LL-FQTM             PIC S9(04) COMP VALUE +0000.
           05  WRK-OUT-ZZ-FQTM             PIC S9(04) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-FQTM         PIC  X(37)      VALUE SPACES.
           05  WRK-OUT-COMANDO-FQTM        PIC  X(68)      VALUE SPACES.
           05  WRK-OUT-DADOS-FQTM.
               10  WRK-OUT-TIMEST-FQTM     PIC  X(26)      VALUE SPACES.
               10  WRK-OUT-TPOPCAO-FQTM    PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-TIPOCON-FQTM    PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-ULTPAG-FQTM     PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-PASSO-FQTM      PIC  X(01)      VALUE SPACES.
               10  WRK-OUT-CHAMADO-FQTM    PIC  X(08)      VALUE SPACES.
               10  WRK-OUT-PAGINA-FQTM-X.
                   15  WRK-OUT-PAGINA-FQTM PIC  9(04)      VALUE ZEROS.
      *
               10  WRK-OUT-NCLIE-FQTM      PIC  X(40)      VALUE SPACES.
               10  WRK-OUT-CCNPJ-FQTM      PIC  X(17)      VALUE SPACES.
               10  WRK-OUT-CAGCT-FQTM      PIC  X(23)      VALUE SPACES.
      *
               10  WRK-OUT-CPROD-FQTM-X.
                   15  WRK-OUT-CPROD-FQTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NPROD-FQTM      PIC  X(12)      VALUE SPACES.
      *
               10  WRK-OUT-CSUBP-FQTM-X.
                   15  WRK-OUT-CSUBP-FQTM  PIC  9(03)      VALUE ZEROS.
               10  WRK-OUT-NSUBP-FQTM      PIC  X(10)      VALUE SPACES.
      *
               10  WRK-OUT-COPER-FQTM-X.
                   15  WRK-OUT-COPER-FQTM  PIC  9(13)      VALUE ZEROS.
                   15  FILLER REDEFINES WRK-OUT-COPER-FQTM.
                       20  WRK-OUT-DANO-OPER-FQTM     PIC  9(04).
                       20  WRK-OUT-NSEQ-OPER-FQTM     PIC  9(09).
      *
               10  WRK-OUT-NSITU-FQTM      PIC  X(15)      VALUE SPACES.
               10  WRK-OUT-NSIT2-FQTM      PIC  X(09)      VALUE SPACES.
               10  WRK-OUT-CSITU-FQTM-X.
                   15  WRK-OUT-CSITU-FQTM  PIC  9(10)      VALUE ZEROS.
               10  WRK-OUT-DTINC-FQTM      PIC  X(10)      VALUE SPACES.
               10  WRK-OUT-VOPER-FQTM.
                   15  WRK-OUT-VOPER-FQTM-E     PIC ZZ.ZZZ.ZZZ.ZZ9,99.
      *
               10  WRK-OUT-LINHAS-FQTM OCCURS 08 TIMES.
                   15  WRK-OUT-SELEC-ATTR-FQTM
                                           PIC S9(04) COMP VALUE ZEROS.
                   15  WRK-OUT-SELEC-FQTM
                                           PIC  X(01)      VALUE SPACES.
                   15  WRK-OUT-DTHIS-FQTM  PIC  X(10)      VALUE SPACES.
                   15  WRK-OUT-DTADT-FQTM  PIC  X(10)      VALUE SPACES.
                   15  WRK-OUT-TIMES-FQTM  PIC  X(26)      VALUE SPACES.
           05  WRK-OUT-MENSA-FQTM          PIC  X(79)      VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA PARA COMUNICACAO MODULO DCOM5526 ***'.
      *----------------------------------------------------------------*
       01  5526-ENTRADA.
           03  5526E-COD-RETORNO              PIC  X(004).
           03  5526E-MSG-RETORNO              PIC  X(079).
           03  5526E-RESTART                  PIC  9(005).
           03  5526E-FLAG                     PIC  X(001).
           03  5526E-START-COUNT              PIC  X(001).
           03  5526E-CFUNC-BDSCO              PIC  9(009).
           03  5526E-CTERM                    PIC  X(008).
           03  5526E-IRSUMO-EVNTO-LOG         PIC  X(020).
           03  5526E-DANO-OPER-DESC           PIC  9(004).
           03  5526E-NSEQ-OPER-DESC           PIC  9(009).

       01  5526-SAIDA.
           03  5526S-HEADER.
               05  5526S-COD-RETORNO          PIC  X(004).
               05  5526S-MSG-RETORNO          PIC  X(079).
               05  5526S-RESTART              PIC  9(005).
               05  5526S-FLAG                 PIC  X(001).
               05  5526S-QTDE-RETORNADA       PIC  9(003).
               05  5526S-COUNT                PIC  9(009).
           03  5526S-DADOS.
               05  5526S-TABELA-SAIDA         OCCURS 30 TIMES.
                   10  5526S-DANO-OPER-DESC   PIC  9(004).
                   10  5526S-NSEQ-OPER-DESC   PIC  9(009).
                   10  5526S-CPRODT           PIC  9(003).
                   10  5526S-IABREV-PRODT     PIC  X(012).
                   10  5526S-CSPROD-DESC-COML PIC  9(003).
                   10  5526S-IRSUMO-SPROD-DESC PIC  X(010).
                   10  5526S-DINIC-OPER-DESC  PIC  X(010).
                   10  5526S-VOPER-DESC-COML  PIC  9(015)V99.
                   10  5526S-CSIT-FNAL-DESC   PIC  9(003).
                   10  5526S-IRSUMO-SIT-DESC  PIC  X(015).
                   10  5526S-DATA-ADTTO       PIC  X(010).
                   10  5526S-TIMESTAMP        PIC  X(026).
           03  5526S-CONSISTENCIA REDEFINES 5526S-DADOS.
               05  5526S-TAMANHO-ERRO         PIC  9(001).
               05  5526S-TABELA-ERROS         OCCURS 003 TIMES.
                   07  5526S-CAMPOS-ERROS     PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DCOM6437 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA PARA COMUNICACAO MODULO DCOM5537 ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMJ4'.
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
           '*** AREA COMU IMS ***'.
      *----------------------------------------------------------------*
       COPY I#DCOMJ2.
      *
      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** DCOM0617 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                     TO WRK-OUT-DADOS-FQTM
                                              WRK-OUT-SENHAS-FQTM
                                              WRK-OUT-COMANDO-FQTM
                                              WRK-AREA-COMUN-TOTAL

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

           MOVE 'DCOMFQTM'                 TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM0617'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMFQTM
           END-IF.

           IF  WRK-TELA                    EQUAL 'DCOMFQTM'
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
           MOVE COMU-TIMESTAMP-FQTM        TO WRK-OUT-TIMEST-FQTM
                                              WRK-INP-TIMEST-FQTM
                                              WRK-ENT-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-OPCAO                  TO WRK-INP-TPOPCAO-FQTM
                                              WRK-OUT-TPOPCAO-FQTM .

           INITIALIZE 5537-ENTRADA-ROTEADOR

           MOVE 'H'                     TO  5537-ENT-FLAG
           MOVE WRK-COD-USER            TO  5537-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM            TO  5537-ENT-CTERM
           MOVE WRK-DANO-OPER-DESC      TO  5537-ENT-DANO-OPER-DESC
                                            WRK-INP-DANO-OPER-FQTM
                                            WRK-OUT-DANO-OPER-FQTM
           MOVE WRK-NSEQ-OPER-DESC      TO  5537-ENT-NSEQ-OPER-DESC
                                            WRK-INP-NSEQ-OPER-FQTM
                                            WRK-OUT-NSEQ-OPER-FQTM

           MOVE 'DCOM5537'              TO  WRK-MODULO
           CALL WRK-MODULO    USING     5537-ENTRADA-ROTEADOR
                                        5537-SAIDA-ROTEADOR
                                        ERRO-AREA
                                        WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO

               WHEN '0000'
                    IF  5537-SAI-FLAG             EQUAL 'O'
                        MOVE '*'                  TO WRK-INCONSIS
                        MOVE 'OPERACAO NAO PERMITE A OPCAO SELECIONADA,
      -                      'OPERACAO ESTA NA OFICIAL'
                                                  TO WRK-OUT-MENSA-FQTM
                        PERFORM 2220-RETORNAR-CHAMADOR
                    ELSE
BSI001                  IF  WRK-COMU-OPCAO-EMIT   NOT EQUAL 1 AND 2
BSI001                                                        AND 3
BSI001                      PERFORM 1253-10-VALIDAR-PERFIL
BSI001                  END-IF
                        IF  WRK-INCONSIS          EQUAL SPACES
                            PERFORM 2110-MONTAR-TELA
                        ELSE
                            PERFORM 2220-RETORNAR-CHAMADOR
                            GO TO 2100-99-FIM
                        END-IF
                    END-IF

               WHEN '0003'
                    MOVE '*'                  TO WRK-INCONSIS
                    MOVE 5537-SAI-MSG-RETORNO TO WRK-OUT-MENSA-FQTM
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE '*'                  TO WRK-INCONSIS
                    MOVE 5537-SAI-MSG-RETORNO TO WRK-OUT-MENSA-FQTM
                    PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0617'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMEST-FQTM        TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

      *----------------------------------------------------------------*
       2100-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1253-10-VALIDAR-PERFIL          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE 'DCOM6437'             TO WRK-MODULO.
           MOVE WRK-COD-USER-R         TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6437-AGEUSU-ENV.
           MOVE 5537-SAI-CAG-BCRIA     TO ROTENT-6437-AGEPESQ-ENV.
           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.

           CALL WRK-MODULO          USING ROTENT-6437-ENVIO
                                          ROTSAI-6437-RETORNO
                                          ERRO-AREA
                                          WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET    EQUAL 99
               MOVE  'DB2'            TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6437-COD-RET        NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET      EQUAL 05 OR
               ROTSAI-6437-RESUL-RET      EQUAL 04
               MOVE '*'                   TO WRK-INCONSIS
               MOVE 'USUARIO NAO AUTORIZADO'
                                          TO WRK-OUT-MENSA-FQTM
           END-IF.

      *----------------------------------------------------------------*
       1253-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DOS DADOS DA TELA                               *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                    SECTION.
      *----------------------------------------------------------------*

           MOVE 5537-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NCLIE-FQTM

           MOVE 5537-SAI-CCNPJ-CPF         TO WRK-CCNPJ-CPF-E
           MOVE 5537-SAI-CFLIAL-CNPJ       TO WRK-CFQIAL-CNPJ-E
           MOVE 5537-SAI-CCTRL-CNPJ-CPF    TO WRK-CCTRL-CNPJ-CPF-E
           MOVE WRK-CNPJ-E                 TO WRK-OUT-CCNPJ-FQTM

           MOVE 5537-SAI-CBCO              TO WRK-CBCO
           MOVE 5537-SAI-CAG-BCRIA         TO WRK-CAG-BCRIA
           MOVE 5537-SAI-CCTA-BCRIA-CLI    TO WRK-CCTA-BCRIA-CLI
           MOVE WRK-BCO-AGE-CTA            TO WRK-OUT-CAGCT-FQTM

           MOVE 5537-SAI-CPRODT            TO WRK-OUT-CPROD-FQTM
           MOVE 5537-SAI-IABREV-PRODT      TO WRK-OUT-NPROD-FQTM
           MOVE 5537-SAI-CSPROD-DESC-COML  TO WRK-OUT-CSUBP-FQTM
           MOVE 5537-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-NSUBP-FQTM

           MOVE WRK-OPERACAO               TO WRK-OUT-COPER-FQTM

           MOVE 5537-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-NSITU-FQTM

           IF  5537-SAI-CCONTR-CONVE-DESC NOT EQUAL ZEROS
               IF  5537-SAI-CELMTO-DESC-COML  EQUAL 8
                   MOVE 'CONV GERAL..:'       TO WRK-OUT-NSIT2-FQTM
               ELSE
                   MOVE 'CONV CLIENTE:'       TO WRK-OUT-NSIT2-FQTM
               END-IF
               MOVE 5537-SAI-CCONTR-CONVE-DESC
                                          TO WRK-OUT-CSITU-FQTM
           ELSE
               IF  5537-SAI-CCONTR-LIM-DESC NOT EQUAL ZEROS
                   MOVE 'CONTR LIMITE:'   TO WRK-OUT-NSIT2-FQTM
                   MOVE 5537-SAI-CCONTR-LIM-DESC
                                          TO WRK-OUT-CSITU-FQTM
               ELSE
                   IF  5537-SAI-NSMULA-OPER-DESC GREATER ZEROS
                       MOVE 'SIMULACAO...:'      TO WRK-OUT-NSIT2-FQTM
                       MOVE 5537-SAI-NSMULA-OPER-DESC
                                         TO WRK-OUT-CSITU-FQTM
                   END-IF
               END-IF
           END-IF.

           MOVE 5537-SAI-VOPER-DESC-COML   TO WRK-OUT-VOPER-FQTM-E

           STRING   5537-SAI-DINIC-OPER-DESC(1:2) '/'
                    5537-SAI-DINIC-OPER-DESC(4:2) '/'
                    5537-SAI-DINIC-OPER-DESC(7:4) DELIMITED BY SIZE
                                         INTO WRK-OUT-DTINC-FQTM

           MOVE ZEROS                      TO WRK-RESTART
           MOVE 1                          TO WRK-OUT-PAGINA-FQTM

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0617'
                   COMPUTE WRK-RESTART       = (WRK-PAGINA(IND-1) - 1)
                                             * 8
                   MOVE WRK-PAGINA(IND-1)    TO WRK-OUT-PAGINA-FQTM
               END-IF
           END-PERFORM.

           PERFORM 2120-ACESSAR-DCOM5526 .
           IF   WRK-INCONSIS          NOT  EQUAL '*'
                MOVE SG-MENSAGEM(61)            TO WRK-OUT-MENSA-FQTM
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2120-ACESSAR-DCOM5526               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                         5526-ENTRADA
                                              5526-SAIDA.

           MOVE ZEROS                      TO 5526E-FLAG
           MOVE WRK-RESTART                TO 5526E-RESTART
           MOVE WRK-COD-USER               TO 5526E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO 5526E-CTERM
           MOVE WRK-DANO-OPER-DESC         TO 5526E-DANO-OPER-DESC
           MOVE WRK-NSEQ-OPER-DESC         TO 5526E-NSEQ-OPER-DESC

           MOVE 'DCOM5526'                 TO WRK-MODULO

           CALL WRK-MODULO             USING  5526-ENTRADA
                                              5526-SAIDA
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE 5526S-COD-RETORNO
              WHEN '0000'
                PERFORM 2121-MONTAR-LISTA  VARYING WRK-IND FROM 1 BY 1
                                           UNTIL WRK-IND GREATER 08

                IF  5526S-DATA-ADTTO(9)  EQUAL SPACES OR LOW-VALUES
                    MOVE '*'               TO WRK-INP-ULTPAG-FQTM
                                              WRK-OUT-ULTPAG-FQTM
                ELSE
                    MOVE SPACES            TO WRK-INP-ULTPAG-FQTM
                                              WRK-OUT-ULTPAG-FQTM
                END-IF

              WHEN '0003'
              WHEN '0100'
                MOVE '*'                   TO WRK-OUT-ULTPAG-FQTM
                                              WRK-INCONSIS
                MOVE 5526S-MSG-RETORNO     TO WRK-OUT-MENSA-FQTM
                PERFORM 2220-RETORNAR-CHAMADOR

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                MOVE 5526S-MSG-RETORNO     TO WRK-OUT-MENSA-FQTM
                MOVE '*'                   TO WRK-INCONSIS

           END-EVALUATE .

      *----------------------------------------------------------------*
       2120-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2121-MONTAR-LISTA                   SECTION.
      *----------------------------------------------------------------*


           IF  5526S-DATA-ADTTO(WRK-IND)   EQUAL SPACES OR LOW-VALUES
               MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-FQTM(WRK-IND)
           ELSE
               MOVE WRK-49345  TO WRK-OUT-SELEC-ATTR-FQTM(WRK-IND)
               MOVE 5526S-TIMESTAMP   (WRK-IND)
                                         TO WRK-OUT-TIMES-FQTM(WRK-IND)
               STRING 5526S-TIMESTAMP (WRK-IND)(9:2) '/'
                      5526S-TIMESTAMP (WRK-IND)(6:2) '/'
                      5526S-TIMESTAMP (WRK-IND)(1:4) DELIMITED BY SIZE
                                       INTO WRK-OUT-DTHIS-FQTM(WRK-IND)
               STRING 5526S-DATA-ADTTO(WRK-IND)(1:2) '/'
                      5526S-DATA-ADTTO(WRK-IND)(4:2) '/'
                      5526S-DATA-ADTTO(WRK-IND)(7:4) DELIMITED BY SIZE
                                       INTO WRK-OUT-DTADT-FQTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOMFQTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMFQTM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-FQTM

           PERFORM 2205-DEVOLVER-TELA
           PERFORM 2210-DEVOLVER-TELA-VARIA

           EVALUATE TRUE
             WHEN WRK-INP-PFK-FQTM         EQUAL 'H' OR
                  (WRK-INP-PFK-FQTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FQTM    EQUAL 'PFK01')
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-INP-MENSA-FQTM TO  WRK-OUT-MENSA-FQTM

             WHEN WRK-INP-PFK-FQTM         EQUAL '3' OR
                   (WRK-INP-PFK-FQTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FQTM   EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FQTM       EQUAL 'X' OR
                   (WRK-INP-PFK-FQTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FQTM   EQUAL 'PFK05')
                  PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-FQTM       EQUAL '7' OR
                   (WRK-INP-PFK-FQTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FQTM   EQUAL 'PFK07')
                  PERFORM 2250-TRATAR-PFK07

               WHEN WRK-INP-PFK-FQTM       EQUAL '8' OR
                   (WRK-INP-PFK-FQTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FQTM   EQUAL 'PFK08')
                  PERFORM 2260-TRATAR-PFK08

               WHEN WRK-INP-PFK-FQTM       EQUAL 'Z' OR
                   (WRK-INP-PFK-FQTM       EQUAL '.' AND
                    WRK-INP-COMANDO-FQTM   EQUAL 'PFK10')
                  PERFORM 2240-MENU-DCOM

             WHEN (WRK-INP-PFK-FQTM        EQUAL '.' AND
                   WRK-INP-COMANDO-FQTM    EQUAL SPACES)
                   PERFORM 2270-TRATAR-ENTER

             WHEN  OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE SG-MENSAGEM(38)    TO  WRK-OUT-MENSA-FQTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA FIXA                                         *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-FQTM        TO WRK-OUT-SENHAS-FQTM
           MOVE WRK-INP-TPOPCAO-FQTM       TO WRK-OUT-TPOPCAO-FQTM
           MOVE WRK-INP-TIPOCON-FQTM       TO WRK-OUT-TIPOCON-FQTM
           MOVE WRK-INP-PASSO-FQTM         TO WRK-OUT-PASSO-FQTM
           MOVE WRK-INP-CHAMADO-FQTM       TO WRK-OUT-CHAMADO-FQTM
           MOVE WRK-INP-TIMEST-FQTM        TO WRK-OUT-TIMEST-FQTM
           MOVE WRK-INP-ULTPAG-FQTM        TO WRK-OUT-ULTPAG-FQTM
           MOVE WRK-INP-PAGINA-FQTM-X      TO WRK-OUT-PAGINA-FQTM-X

           MOVE WRK-INP-NCLIE-FQTM         TO WRK-OUT-NCLIE-FQTM
           MOVE WRK-INP-CCNPJ-FQTM         TO WRK-OUT-CCNPJ-FQTM
           MOVE WRK-INP-CAGCT-FQTM         TO WRK-OUT-CAGCT-FQTM
           MOVE WRK-INP-CPROD-FQTM-X       TO WRK-OUT-CPROD-FQTM-X
           MOVE WRK-INP-NPROD-FQTM         TO WRK-OUT-NPROD-FQTM
           MOVE WRK-INP-CSUBP-FQTM-X       TO WRK-OUT-CSUBP-FQTM-X
           MOVE WRK-INP-NSUBP-FQTM         TO WRK-OUT-NSUBP-FQTM
           MOVE WRK-INP-COPER-FQTM-X       TO WRK-OUT-COPER-FQTM-X
           MOVE WRK-INP-NSITU-FQTM         TO WRK-OUT-NSITU-FQTM
           MOVE WRK-INP-NSIT2-FQTM         TO WRK-OUT-NSIT2-FQTM
           MOVE WRK-INP-CSITU-FQTM-X       TO WRK-OUT-CSITU-FQTM-X
           MOVE WRK-INP-DTINC-FQTM         TO WRK-OUT-DTINC-FQTM
           MOVE WRK-INP-VOPER-FQTM         TO WRK-OUT-VOPER-FQTM

           MOVE WRK-INP-MENSA-FQTM         TO WRK-OUT-MENSA-FQTM.

      *----------------------------------------------------------------*
       2205-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     DEVOLVER TELA VARIA                                        *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA            SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM 1 BY 1
                      UNTIL  WRK-IND  GREATER 8

               IF  WRK-INP-DTADT-FQTM(WRK-IND)  EQUAL SPACES OR
                                                      LOW-VALUES
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-FQTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-FQTM(WRK-IND)
                                     TO WRK-OUT-SELEC-FQTM(WRK-IND)
                   MOVE WRK-INP-DTADT-FQTM(WRK-IND)
                                     TO WRK-OUT-DTADT-FQTM(WRK-IND)
                   MOVE WRK-INP-DTHIS-FQTM(WRK-IND)
                                     TO WRK-OUT-DTHIS-FQTM(WRK-IND)
                   MOVE WRK-INP-TIMES-FQTM(WRK-IND)
                                     TO WRK-OUT-TIMES-FQTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0103                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMEST-FQTM        TO WRK-ENT-TIMESTAMP
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
           MOVE WRK-INP-TIMEST-FQTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FQTM
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0617'           TO COMU-TRANSACAO-FQTM.
           MOVE LENGTH OF COMU-AREA-IMS
                                     TO COMU-LL-FQTM

           IF  WRK-INCONSIS          EQUAL  '*'
               MOVE WRK-OUT-MENSA-FQTM
                                     TO COMU-MENSAGEM-FQTM
           END-IF.

           MOVE COMU-AREA-IMS        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2230-MENU-ROTINAS                   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0617'                 TO WRK-TELA
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

           INITIALIZE COMU-AREA-PFK
           MOVE LENGTH OF COMU-AREA-PFK    TO COMU-LL-PFK
           MOVE WRK-CHNG                   TO WRK-FUNCAO
           MOVE 'DCOM0617'                 TO WRK-TELA
                                              COMU-TRANSACAO-PFK
           MOVE 'A'                        TO COMU-PFK-PFK
           MOVE COMU-AREA-PFK              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2240-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2250-TRATAR-PFK07                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-FQTM         EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-FQTM(1)
               MOVE SG-MENSAGEM(28)        TO WRK-OUT-MENSA-FQTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE SPACES                    TO WRK-INP-ULTPAG-FQTM
                                             WRK-OUT-ULTPAG-FQTM.

           COMPUTE WRK-RESTART         = (WRK-INP-PAGINA-FQTM - 2) * 8

           MOVE WRK-INP-DANO-OPER-FQTM     TO WRK-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-FQTM     TO WRK-NSEQ-OPER-DESC .
           MOVE    SG-MENSAGEM(01)        TO WRK-OUT-MENSA-FQTM

           PERFORM 2120-ACESSAR-DCOM5526 .
           COMPUTE WRK-OUT-PAGINA-FQTM     = WRK-INP-PAGINA-FQTM - 1.

      *----------------------------------------------------------------*
       2250-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR TELA POSTERIOR                                    *
      *----------------------------------------------------------------*
       2260-TRATAR-PFK08                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-FQTM         EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE WRK-49345              TO WRK-OUT-SELEC-ATTR-FQTM(1)
               MOVE SG-MENSAGEM(29)        TO WRK-OUT-MENSA-FQTM
               GO TO 2260-99-FIM
           END-IF.

           MOVE SPACES                    TO WRK-INP-ULTPAG-FQTM
                                             WRK-OUT-ULTPAG-FQTM.

           COMPUTE WRK-RESTART         = (WRK-INP-PAGINA-FQTM) * 8

           MOVE WRK-INP-DANO-OPER-FQTM     TO WRK-DANO-OPER-DESC
           MOVE WRK-INP-NSEQ-OPER-FQTM     TO WRK-NSEQ-OPER-DESC .

           PERFORM 2120-ACESSAR-DCOM5526 .

           IF  WRK-INCONSIS                EQUAL '*'
               GO TO 2260-99-FIM
           END-IF

           IF  WRK-OUT-ULTPAG-FQTM         EQUAL '*'

               MOVE SG-MENSAGEM(17)        TO WRK-OUT-MENSA-FQTM
           ELSE
               MOVE SG-MENSAGEM(01)        TO WRK-OUT-MENSA-FQTM
           END-IF .

           COMPUTE WRK-OUT-PAGINA-FQTM     = WRK-INP-PAGINA-FQTM + 1.

      *----------------------------------------------------------------*
       2260-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER                   SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                    TO WRK-INCONSIS

           PERFORM 2271-CONSISTIR-FISICA.

      *----------------------------------------------------------------*
       2270-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                      TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 8

               IF  WRK-INP-SELEC-FQTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'                TO WRK-INCONSIS
                   MOVE WRK-49369          TO
                        WRK-OUT-SELEC-ATTR-FQTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)    TO WRK-OUT-MENSA-FQTM
               ELSE
                   IF  WRK-INP-SELEC-FQTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369      TO
                            WRK-OUT-SELEC-ATTR-FQTM(WRK-IND)
                       ADD 1               TO WCT-SELEC
                       MOVE WRK-IND        TO WIND-SEL
                   END-IF
               END-IF
           END-PERFORM.

           IF WRK-INCONSIS  EQUAL  '*'
              GO TO 2271-99-FIM
           END-IF.

           IF  WCT-SELEC                   EQUAL 0 OR GREATER 1
               MOVE '*'                    TO WRK-INCONSIS
               IF  WCT-SELEC               EQUAL 0
                   MOVE SG-MENSAGEM(53)    TO WRK-OUT-MENSA-FQTM
               ELSE
                 IF  WCT-SELEC             GREATER 1
                     MOVE SG-MENSAGEM(39)  TO WRK-OUT-MENSA-FQTM
                 END-IF
               END-IF
           ELSE
               PERFORM 2272-TRANSFERIR-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                        EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMEST-FQTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FQTM

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)      EQUAL SPACES
                   MOVE 'DCOM0617'          TO WRK-CHAMADOR(IND-1)
                                               COMU-TRANSACAO-FQTM
                   MOVE WRK-INP-PAGINA-FQTM TO WRK-PAGINA(IND-1)
                   MOVE 99                  TO IND-1
               END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 8

               IF  WRK-INP-SELEC-FQTM(WRK-IND) EQUAL 'X'
                   MOVE WRK-INP-DTADT-FQTM(WRK-IND)
                                           TO WRK-DATA-ADITAMENTO
                   MOVE WRK-INP-TIMES-FQTM(WRK-IND)
                                           TO WRK-TIMESTAMP
               END-IF
           END-PERFORM.

           MOVE 'DCOM0618'                 TO WRK-TELA

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995

           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL-FQTM.
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660             SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FQTM  TO WRK-OUT-LL-FQTM
                                              WRK-660-LL-MENS-FQTM.
           MOVE LENGTH OF WRK-660-DCOMFQTM TO WRK-660-LL-AREA-FQTM.

           CALL 'BRAD0660'             USING  WRK-OUTPUT-FQTM
                                              WRK-660-DCOMFQTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE            TO WRK-RETURN-CODE
               MOVE 10                     TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT                   TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FQTM            TO WRK-MENSAGEM.

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
       7000-ACESSAR-DCOM5537               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                         5537-ENTRADA-ROTEADOR
                                              5537-SAIDA-ROTEADOR

           MOVE 'H'                        TO 5537-ENT-FLAG
           MOVE ZEROS                      TO 5537-ENT-RESTART
           MOVE WRK-COD-USER               TO 5537-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM               TO 5537-ENT-CTERM
           MOVE WRK-DANO-OPER-DESC         TO 5537-ENT-DANO-OPER-DESC
           MOVE WRK-NSEQ-OPER-DESC         TO 5537-ENT-NSEQ-OPER-DESC

           MOVE 'DCOM5537'                 TO WRK-MODULO

           CALL WRK-MODULO             USING  5537-ENTRADA-ROTEADOR
                                              5537-SAIDA-ROTEADOR
                                              ERRO-AREA
                                              WRK-SQLCA

           EVALUATE 5537-SAI-COD-RETORNO
              WHEN '0000'
                PERFORM 2110-MONTAR-TELA

              WHEN '0003'
              WHEN '0100'
                MOVE '*'                   TO WRK-OUT-ULTPAG-FQTM
                MOVE SG-MENSAGEM(63)       TO WRK-OUT-MENSA-FQTM

                MOVE '9'                   TO WRK-OUT-PASSO-FQTM

              WHEN '0099'
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                MOVE 'APL'                 TO ERR-TIPO-ACESSO
                PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

      *----------------------------------------------------------------*
       7000-99-FIM.                        EXIT.
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
       9999-PROCESSAR-ROTINA-ERRO          SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0617'             TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'             TO ERR-PGM
               MOVE 'DCOM0617'             TO ERR-MODULO
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
