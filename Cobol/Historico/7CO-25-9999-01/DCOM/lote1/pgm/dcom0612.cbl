      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0612.
       AUTHOR.     FABRICA ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0612                                    *
      *    PROGRAMADOR.:   FABRICA - ALTRAN.                           *
      *    ANALISTA....:   EMERSON ARTUR FRANCO - ALTRAN.              *
      *    DATA........:   21/01/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR OPERACAO - DEMOSTRATIVO           *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFLTM - CONSULTAR OPERACAO - DEMONSTRATIVO               *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0605           CHAMA: DCOM5537            *
      *    RETORNA PARA: DCOM0604                                      *
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

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0612 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  IND-1                   PIC  9(005)         VALUE ZEROS.
           05  WRK-VLR-S               PIC S9(014)V99      VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-NAO-ACHOU           PIC  X(001)         VALUE 'N'.

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
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)         VALUE SPACES.
           05  WRK-TELA                PIC  X(008)         VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)         VALUE
               'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01 WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DCOM6437 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFLTM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-FLTM.
           03  WRK-INP-LL-FLTM              PIC 9(04) COMP.
           03  WRK-INP-ZZ-FLTM              PIC 9(04) COMP.
           03  WRK-INP-TRANCODE-FLTM        PIC X(08).
           03  FILLER                       PIC X(06).
           03  WRK-INP-PFK-FLTM             PIC X(01).
           03  WRK-INP-SENHAS-FLTM          PIC X(37).
           03  WRK-INP-COMANDO-FLTM         PIC X(68).
           03  WRK-INP-NOMECLI-FLTM         PIC X(40).
           03  WRK-INP-TPOPCAO-FLTM         PIC 9(01).
           03  WRK-INP-CNPJCPF-FLTM         PIC X(19).
           03  WRK-INP-DADOSBC-FLTM.
               05  WRK-INP-BANCO-FLTM       PIC 9(03).
               05  FILLER                   PIC X(01) VALUE '/'.
               05  WRK-INP-AGENCIA-FLTM     PIC 9(05).
               05  FILLER                   PIC X(01) VALUE '/'.
               05  WRK-INP-CONTA-FLTM       PIC 9(13).
           03  WRK-INP-CODPROD-FLTM         PIC X(03).
           03  WRK-INP-DESPROD-FLTM         PIC X(12).
           03  WRK-INP-CODSUBP-FLTM         PIC X(03).
           03  WRK-INP-DESSUBP-FLTM         PIC X(12).
           03  WRK-INP-OPERACAO-FLTM-X.
               05  WRK-INP-DANO-OPER-FLTM   PIC 9(04).
               05  WRK-INP-NSEQ-OPER-FLTM   PIC 9(09).
           03  WRK-INP-SITU1-FLTM           PIC X(15).
           03  WRK-INP-SITU2-FLTM           PIC X(13).
           03  WRK-INP-SITU3-FLTM           PIC X(09).
           03  WRK-INP-SITU4-FLTM           PIC X(13).
           03  WRK-INP-SITU5-FLTM           PIC X(09).
           03  WRK-INP-TPDES-FLTM           PIC X(03).
           03  WRK-INP-TIMEST-FLTM          PIC X(26).
           03  WRK-INP-PGMANT-FLTM          PIC X(08).
           03  WRK-INP-CARTCONS-FLTM        PIC X(03).
           03  WRK-INP-NUM1CONS-FLTM        PIC X(04).
           03  WRK-INP-NUM2CONS-FLTM        PIC X(05).
           03  WRK-INP-NUM3CONS-FLTM        PIC X(09).
           03  WRK-INP-DOPERAC-FLTM         PIC X(10).
           03  WRK-INP-VLROPER-FLTM         PIC X(14).
           03  WRK-INP-TPTAXA-FLTM          PIC X(15).
           03  WRK-INP-VLRTARO-FLTM         PIC X(14).
ANA        03  WRK-INP-TXJUROS-FLTM         PIC X(09).
           03  WRK-INP-TXDESC-FLTM          PIC X(04).
           03  WRK-INP-VLRTART-FLTM         PIC X(14).
           03  WRK-INP-DVCTFIM-FLTM         PIC X(10).
           03  WRK-INP-VLRJUROS-FLTM        PIC X(14).
           03  WRK-INP-PRZMEDIO-FLTM        PIC X(03).
           03  WRK-INP-VLRIOF-FLTM          PIC X(14).
           03  WRK-INP-QTDTIT-FLTM          PIC X(06).
           03  WRK-INP-VLRLIQ-FLTM          PIC X(15).
           03  WRK-INP-GARANTIA-FLTM        PIC X(20).
           03  WRK-INP-CONTRC-FLTM.
               05  WRK-INP-CONTRC-FLTM-N    PIC 9(07).
           03  WRK-INP-CODMEIO-FLTM         PIC X(03).
           03  WRK-INP-MEIOENT-FLTM         PIC X(15).
BRQ001     03  WRK-INP-LIBMID-FLTM          PIC X(03)      VALUE SPACES.
           03  WRK-INP-MENSA-FLTM           PIC X(79).

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFLTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-FLTM.
ANA        03  WRK-OUT-LL-FLTM              PIC 9(04) COMP VALUE 0000.
           03  WRK-OUT-ZZ-FLTM              PIC 9(04) COMP VALUE ZEROS.
           03  WRK-OUT-SENHAS-FLTM          PIC X(37).
           03  WRK-OUT-COMANDO-FLTM         PIC X(68).
           03  WRK-OUT-DADOS-FLTM.
               05  WRK-OUT-NOMECLI-FLTM         PIC X(40).
               05  WRK-OUT-TPOPCAO-FLTM         PIC 9(01).
               05  WRK-OUT-CNPJCPF-FLTM         PIC X(19).
               05  WRK-OUT-DADOSBC-FLTM.
                   07  WRK-OUT-BANCO-FLTM       PIC 9(03).
                   07  WRK-OUT-BARRA-1-FLTM     PIC X(01) VALUE '/'.
                   07  WRK-OUT-AGENCIA-FLTM     PIC 9(05).
                   07  WRK-OUT-BARRA-2-FLTM     PIC X(01) VALUE '/'.
                   07  WRK-OUT-CONTA-FLTM       PIC 9(13).
               05  WRK-OUT-CODPROD-FLTM-X.
                   07  WRK-OUT-CODPROD-FLTM     PIC 9(03).
               05  WRK-OUT-DESPROD-FLTM         PIC X(12).
               05  WRK-OUT-CODSUBP-FLTM-X.
                   07  WRK-OUT-CODSUBP-FLTM     PIC 9(03).
               05  WRK-OUT-DESSUBP-FLTM         PIC X(12).
               05  WRK-OUT-OPERACAO-FLTM-X.
                   07  WRK-OUT-DANO-OPER-FLTM   PIC 9(04).
                   07  WRK-OUT-NSEQ-OPER-FLTM   PIC 9(09).
               05  WRK-OUT-SITU1-FLTM           PIC X(15).
               05  WRK-OUT-SITU2-FLTM           PIC X(13).
               05  WRK-OUT-SITU3-FLTM           PIC X(09).
               05  WRK-OUT-SITU4-FLTM           PIC X(13).
               05  WRK-OUT-SITU5-FLTM           PIC X(09).
               05  WRK-OUT-TPDES-FLTM-X.
                   10  WRK-OUT-TPDES-FLTM       PIC 9(03).
               05  WRK-OUT-TIMEST-FLTM          PIC X(26).
               05  WRK-OUT-PGMANT-FLTM          PIC X(08).
               05  WRK-OUT-CARTCONS-FLTM-X.
                   07  WRK-OUT-CARTCONS-FLTM    PIC X(03).
               05  WRK-OUT-NUM1CONS-FLTM-X.
                   07  WRK-OUT-NUM1CONS-FLTM    PIC 9(04).
               05  WRK-OUT-NUM2CONS-FLTM-X.
                   07  WRK-OUT-NUM2CONS-FLTM    PIC 9(05).
               05  WRK-OUT-NUM3CONS-FLTM-X.
                   07  WRK-OUT-NUM3CONS-FLTM    PIC 9(09).
               05  WRK-OUT-DOPERAC-FLTM         PIC X(10).
               05  WRK-OUT-VLROPER-FLTM-X.
                   07  WRK-OUT-VLROPER-FLTM     PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-TPTAXA-FLTM          PIC X(15).
               05  WRK-OUT-VLRTARO-FLTM-X.
                   07  WRK-OUT-VLRTARO-FLTM     PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-TXJUROS-FLTM-X.
ANA                07  WRK-OUT-TXJUROS-FLTM     PIC ZZ9,99999.
               05  WRK-OUT-TXDESC-FLTM          PIC X(04).
               05  WRK-OUT-VLRTART-FLTM-X.
                   07  WRK-OUT-VLRTART-FLTM     PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-DVCTFIM-FLTM         PIC X(10).
               05  WRK-OUT-VLRJUROS-FLTM-X.
                   07  WRK-OUT-VLRJUROS-FLTM    PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-PRZMEDIO-FLTM-X.
                   07  WRK-OUT-PRZMEDIO-FLTM    PIC 9(03).
               05  WRK-OUT-VLRIOF-FLTM-X.
                   07  WRK-OUT-VLRIOF-FLTM      PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-QTDTIT-FLTM-X.
                   07  WRK-OUT-QTDTIT-FLTM      PIC 999999.
               05  WRK-OUT-VLRLIQ-FLTM-X.
LB                 07  WRK-OUT-VLRLIQ-FLTM      PIC ----.---.--9,99.
               05  WRK-OUT-GARANTIA-FLTM        PIC X(20).
               05  WRK-OUT-CONTRC-FLTM.
                   07  WRK-OUT-CONTRC-FLTM-N    PIC 9(07).
               05  WRK-OUT-CODMEIO-FLTM-X.
                   07  WRK-OUT-CODMEIO-FLTM     PIC 9(03).
               05  WRK-OUT-MEIOENT-FLTM         PIC X(15).
BRQ001         05  WRK-OUT-LIBMID-FLTM          PIC X(03)  VALUE SPACES.
               05  WRK-OUT-MENSA-FLTM           PIC X(79).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFLTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMFLTM.
           03  WRK-660-LL-AREA-FLTM         PIC 9(04) COMP VALUE 0000.
           03  WRK-660-LL-MENSAGEM-FLTM     PIC 9(04) COMP VALUE 0000.
           03  WRK-660-SENHAS-FLTM          PIC 9(04) COMP VALUE 0037.
           03  WRK-660-COMANDO-FLTM         PIC 9(04) COMP VALUE 0068.
           03  WRK-660-NOMECLI-FLTM         PIC 9(04) COMP VALUE 0040.
           03  WRK-660-TPOPCAO-FLTM         PIC 9(04) COMP VALUE 0001.
           03  WRK-660-CNPJCPF-FLTM         PIC 9(04) COMP VALUE 0019.
           03  WRK-660-DADOSBC-FLTM         PIC 9(04) COMP VALUE 0023.
           03  WRK-660-CODPROD-FLTM         PIC 9(04) COMP VALUE 0003.
           03  WRK-660-DESPROD-FLTM         PIC 9(04) COMP VALUE 0012.
           03  WRK-660-CODSUBP-FLTM         PIC 9(04) COMP VALUE 0003.
           03  WRK-660-DESSUBP-FLTM         PIC 9(04) COMP VALUE 0012.
           03  WRK-660-OPERACAO-FLTM        PIC 9(04) COMP VALUE 0013.
           03  WRK-660-SITU1-FLTM           PIC 9(04) COMP VALUE 0015.
           03  WRK-660-SITU2-FLTM           PIC 9(04) COMP VALUE 0013.
           03  WRK-660-SITU3-FLTM           PIC 9(04) COMP VALUE 0009.
           03  WRK-660-SITU4-FLTM           PIC 9(04) COMP VALUE 0013.
           03  WRK-660-SITU5-FLTM           PIC 9(04) COMP VALUE 0009.
           03  WRK-660-TPDES-FLTM           PIC 9(04) COMP VALUE 0003.
           03  WRK-660-TIMEST-FLTM          PIC 9(04) COMP VALUE 0026.
           03  WRK-660-PGMANT-FLTM          PIC 9(04) COMP VALUE 0008.
           03  WRK-660-CARTCONS-FLTM        PIC 9(04) COMP VALUE 0003.
           03  WRK-660-NUM1CONS-FLTM        PIC 9(04) COMP VALUE 0004.
           03  WRK-660-NUM2CONS-FLTM        PIC 9(04) COMP VALUE 0005.
           03  WRK-660-NUM3CONS-FLTM        PIC 9(04) COMP VALUE 0009.
           03  WRK-660-DOPERAC-FLTM         PIC 9(04) COMP VALUE 0010.
           03  WRK-660-VLROPER-FLTM         PIC 9(04) COMP VALUE 0014.
           03  WRK-660-TPTAXA-FLTM          PIC 9(04) COMP VALUE 0015.
           03  WRK-660-VLRTARO-FLTM         PIC 9(04) COMP VALUE 0014.
ANA        03  WRK-660-TXJUROS-FLTM         PIC 9(04) COMP VALUE 0009.
           03  WRK-660-TXDESC-FLTM          PIC 9(04) COMP VALUE 0004.
           03  WRK-660-VLRTART-FLTM         PIC 9(04) COMP VALUE 0014.
           03  WRK-660-DVCTFIM-FLTM         PIC 9(04) COMP VALUE 0010.
           03  WRK-660-VLRJUROS-FLTM        PIC 9(04) COMP VALUE 0014.
           03  WRK-660-PRZMEDIO-FLTM        PIC 9(04) COMP VALUE 0003.
           03  WRK-660-VLRIOF-FLTM          PIC 9(04) COMP VALUE 0014.
           03  WRK-660-QTDTIT-FLTM          PIC 9(04) COMP VALUE 0006.
           03  WRK-660-VLRLIQ-FLTM          PIC 9(04) COMP VALUE 0015.
           03  WRK-660-GARANTIA-FLTM        PIC 9(04) COMP VALUE 0020.
           03  WRK-660-CONTRC-FLTM          PIC 9(04) COMP VALUE 0007.
           03  WRK-660-CODMEIO-FLTM         PIC 9(04) COMP VALUE 0003.
           03  WRK-660-MEIOENT-FLTM         PIC 9(04) COMP VALUE 0015.
BRQ001     03  WRK-660-LIBMID-FLTM          PIC 9(04) COMP VALUE 0003.
           03  WRK-660-MENSA-FLTM           PIC 9(04) COMP VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMAS CHAMADORES           ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-IMS.
           05  COMU-LL-FLTM              PIC S9(004) COMP  VALUE +406.
           05  COMU-ZZ-FLTM              PIC S9(004) COMP  VALUE ZEROS.
           05  COMU-TRANCODE-FLTM.
               10  COMU-TRANSACAO-FLTM   PIC  X(008)       VALUE SPACES.
               10  FILLER                PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-FLTM         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-FLTM          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-FLTM         PIC  X(068)       VALUE SPACES.
           05  COMU-TIMESTAMP-FLTM       PIC  X(026)       VALUE SPACES.
           05  COMU-MENSAGEM-FLTM        PIC  X(079)       VALUE SPACES.
PROC01     05  WRK-COMU-DANO-FLTM        PIC  9(004)       VALUE ZEROS.
PROC01     05  WRK-COMU-NSEQ-FLTM        PIC  9(009)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM4266              ***'.
      *----------------------------------------------------------------*

       01  4266E-ENTRADA.
           03  4266E-COD-RETORNO           PIC  X(004).
           03  4266E-MSG-RETORNO           PIC  X(079).
           03  4266E-RESTART               PIC  9(005).
           03  4266E-FLAG                  PIC  X(001).
           03  4266E-CFUNC-BDSCO           PIC  9(009).
           03  4266E-CTERM                 PIC  X(008).
           03  4266E-DANO-OPER-DESC        PIC  9(004).
           03  4266E-NSEQ-OPER-DESC        PIC  9(009).
FS2511*    03  4266E-CCNPJ-CPF             PIC  9(009).
FS2511     03  4266E-CCNPJ-CPF             PIC  X(009).
FS2511*    03  4266E-CFLIAL-CNPJ           PIC  9(004).
FS2511     03  4266E-CFLIAL-CNPJ           PIC  X(004).

       01  4266S-SAIDA.
           03  4266S-HEADER.
               05  4266S-COD-RETORNO       PIC  X(004).
               05  4266S-MSG-RETORNO       PIC  X(079).
               05  4266S-RESTART           PIC  9(005).
               05  4266S-FLAG              PIC  X(001).
           03  4266S-DADOS.
               05 4266S-CINDCD-FREQ-TX      PIC  X(02).
               05 4266S-CTPO-TX-JURO-DESC   PIC  9(03).
               05 4266S-CSGL-TPO-TX-JURO    PIC  X(06).
               05 4266S-IRSUMO-TPO-TX-JURO  PIC  X(15).
               05 4266S-ITPO-TX-JURO-DESC   PIC  X(30).

           03  4266S-CONSISTENCIA          REDEFINES 4266S-DADOS.
               05  4266S-TAMANHO-ERRO      PIC  9(001).
               05  4266S-TABELA-ERROS      OCCURS 02 TIMES.
                   07  4266S-CAMPOS-ERRO   PIC  9(001).

BRQ001*----------------------------------------------------------------*
BRQ001 01  FILLER                      PIC  X(050)         VALUE
BRQ001     '*** AREA DO MODULO DCOM5490 ***'.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001 01  WRK-5490-ENTRADA.
BRQ001     05  WRK-E5490-COD-RETORNO      PIC  X(004)      VALUE SPACES.
BRQ001     05  WRK-E5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ001     05  WRK-E5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ001     05  WRK-E5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-E5490-START-COUNT      PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-E5490-CFUNC-BDSCO      PIC  9(009)      VALUE ZEROS.
BRQ001     05  WRK-E5490-CTERM            PIC  X(008)      VALUE SPACES.
BRQ001     05  WRK-E5490-CPRODT           PIC  9(003)      VALUE ZEROS.
BRQ001     05  WRK-E5490-CSPROD-DESC-COML PIC  9(003)      VALUE ZEROS.
BRQ001     05  WRK-E5490-CTPO-DESC-COML   PIC  9(003)      VALUE ZEROS.
BRQ001     05  WRK-E5490-NRO-OPERACAO.
BRQ001         10  WRK-E5490-DANO-OPER    PIC  9(004)      VALUE ZEROS.
BRQ001         10  WRK-E5490-NSEQ-OPER    PIC  9(009)      VALUE ZEROS.
BRQ001     05  WRK-E5490-VOPER-DESC-COML  PIC  9(015)V99   VALUE ZEROS.
BRQ001     05  WRK-E5490-FUNCAO           PIC  X(002)      VALUE SPACES.
BRQ001     05  WRK-E5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ001
BRQ001 01  WRK-5490-SAIDA.
BRQ001     05  WRK-S5490-COD-RETORNO      PIC  9(004)      VALUE ZEROS.
BRQ001     05  WRK-S5490-MSG-RETORNO      PIC  X(079)      VALUE SPACES.
BRQ001     05  WRK-S5490-RESTART          PIC  9(005)      VALUE ZEROS.
BRQ001     05  WRK-S5490-FLAG             PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-S5490-LIBRC-MIDIA      PIC  X(001)      VALUE SPACES.
BRQ001     05  WRK-S5490-CADASTRO-BROB    PIC  X(001)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6425 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-6425-ROTEADOR.
           05  ROTENT-6425-COD-RETORNO      PIC  X(004)   VALUE SPACES.
           05  ROTENT-6425-MSG-RETORNO      PIC  X(079)   VALUE SPACES.
           05  ROTENT-6425-RESTART          PIC  9(005)   VALUE ZEROS.
           05  ROTENT-6425-FLAG             PIC  X(001)   VALUE SPACES.
           05  ROTENT-6425-START-COUNT      PIC  X(001)   VALUE SPACES.
           05  ROTENT-6425-CFUNC-BDSCO      PIC  9(009)   VALUE ZEROS.
           05  ROTENT-6425-CTERM            PIC  X(008)   VALUE SPACES.
           05  ROTENT-6425-CGARAN           PIC  9(003)   VALUE ZEROS.

       01  SAIDA-6425-ROTEADOR.
           05  ROTSAI-6425-COD-RETORNO    PIC  X(004)     VALUE SPACES.
           05  ROTSAI-6425-MSG-RETORNO    PIC  X(079)     VALUE SPACES.
           05  ROTSAI-6425-RESTART        PIC  9(005)     VALUE ZEROS.
           05  ROTSAI-6425-FLAG           PIC  X(001)     VALUE SPACES.
           05  ROTSAI-6425-QTDE-RETORNADA PIC  9(003)     VALUE ZEROS.
           05  ROTSAI-6425-COUNT          PIC  9(009)     VALUE ZEROS.
           05  ROTSAI-6425-CGARAN         PIC  9(003)     VALUE ZEROS.
           05  ROTSAI-6425-IGARAN         PIC  X(040)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM5537              ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMJ4'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0612 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005' USING WRK-VERSAO
                                 WRK-FUNCAO
                                 LNK-IO-PCB
                                 LNK-ALT-PCB
                                 WRK-MENSAGEM
                                 WRK-TELA
                                 WRK-COD-USER
                                 WRK-COD-DEPTO.

           IF  RETURN-CODE EQUAL 04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES  TO  WRK-OUT-DADOS-FLTM
                            WRK-OUT-SENHAS-FLTM
                            WRK-OUT-COMANDO-FLTM
                            WRK-INCONSISTENCIA.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMFLTM'  TO  WRK-TELA.

            IF  WRK-TRANSACAO NOT EQUAL 'DCOM0612'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMFLTM
           END-IF.

           IF  WRK-TELA EQUAL 'DCOMFLTM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DE OUTRAS TRANSACOES            *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM            TO  COMU-AREA-IMS

           MOVE COMU-TIMESTAMP-FLTM        TO WRK-OUT-TIMEST-FLTM
                                              WRK-INP-TIMEST-FLTM
                                              WRK-ENT-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-OPCAO                  TO WRK-INP-TPOPCAO-FLTM
                                              WRK-OUT-TPOPCAO-FLTM

           INITIALIZE 5537-ENTRADA-ROTEADOR

           MOVE 'O'                     TO  5537-ENT-FLAG
           MOVE WRK-COD-USER            TO  5537-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM            TO  5537-ENT-CTERM
           MOVE WRK-DANO-OPER-DESC      TO  5537-ENT-DANO-OPER-DESC
                                            WRK-INP-DANO-OPER-FLTM
                                            WRK-OUT-DANO-OPER-FLTM
           MOVE WRK-NSEQ-OPER-DESC      TO  5537-ENT-NSEQ-OPER-DESC
                                            WRK-INP-NSEQ-OPER-FLTM
                                            WRK-OUT-NSEQ-OPER-FLTM

           MOVE 'DCOM5537'              TO  WRK-MODULO
           CALL WRK-MODULO    USING     5537-ENTRADA-ROTEADOR
                                        5537-SAIDA-ROTEADOR
                                        ERRO-AREA
                                        WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO
               WHEN '0000'
                    IF  5537-SAI-FLAG             EQUAL 'H'
                        MOVE 'OPERACAO NAO PERMITE A OPCAO SELECIONADA,
      -                      'OPERACAO ESTA NO HISTORICO'
                                                  TO WRK-OUT-MENSA-FLTM
                        PERFORM 1220-RETORNAR-CHAMADOR
                    ELSE
BSI001                  IF  WRK-COMU-OPCAO-EMIT   NOT EQUAL 1 AND 2
BSI001                                                        AND 3
BSI001                      PERFORM 1253-10-VALIDAR-PERFIL
BSI001                  END-IF
                        IF  WRK-INCONSISTENCIA    EQUAL SPACES
                            PERFORM 1110-MONTAR-TELA
                            MOVE 5537-SAI-CSIT-DESC-COML
                                                  TO  WRK-SITUACAO
                        ELSE
                            PERFORM 1220-RETORNAR-CHAMADOR
                            GO TO 1100-99-FIM
                        END-IF
                    END-IF

               WHEN '0003'
                    MOVE 5537-SAI-MSG-RETORNO TO WRK-OUT-MENSA-FLTM
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 5537-SAI-MSG-RETORNO TO WRK-OUT-MENSA-FLTM
                    PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0612'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

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
               MOVE '*'                   TO WRK-INCONSISTENCIA
               MOVE 'USUARIO NAO AUTORIZADO'
                                          TO WRK-OUT-MENSA-FLTM
           END-IF.

      *----------------------------------------------------------------*
       1253-10-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE 5537-SAI-IPSSOA-DESC-COML TO WRK-INP-NOMECLI-FLTM
                                             WRK-OUT-NOMECLI-FLTM

           STRING 5537-SAI-CCNPJ-CPF(1:3)  '.'
                  5537-SAI-CCNPJ-CPF(4:3)  '.'
                  5537-SAI-CCNPJ-CPF(7:3)  '/'
                  5537-SAI-CFLIAL-CNPJ      '-'
                  5537-SAI-CCTRL-CNPJ-CPF
                                      DELIMITED BY SIZE
                                      INTO  WRK-INP-CNPJCPF-FLTM.
           MOVE WRK-INP-CNPJCPF-FLTM    TO  WRK-OUT-CNPJCPF-FLTM.
           MOVE '/'                     TO  WRK-OUT-BARRA-1-FLTM
                                            WRK-OUT-BARRA-2-FLTM
           MOVE 5537-SAI-CBCO           TO  WRK-INP-BANCO-FLTM
                                            WRK-OUT-BANCO-FLTM
           MOVE 5537-SAI-CAG-BCRIA      TO  WRK-INP-AGENCIA-FLTM
                                            WRK-OUT-AGENCIA-FLTM
           MOVE 5537-SAI-CCTA-BCRIA-CLI TO  WRK-INP-CONTA-FLTM
                                            WRK-OUT-CONTA-FLTM
           MOVE 5537-SAI-CPRODT         TO  WRK-INP-CODPROD-FLTM
                                            WRK-OUT-CODPROD-FLTM
           MOVE 5537-SAI-IABREV-PRODT   TO  WRK-INP-DESPROD-FLTM
                                            WRK-OUT-DESPROD-FLTM
           MOVE 5537-SAI-CSPROD-DESC-COML TO  WRK-INP-CODSUBP-FLTM
                                              WRK-OUT-CODSUBP-FLTM
           MOVE 5537-SAI-IRSUMO-SPROD-DESC TO  WRK-INP-DESSUBP-FLTM
                                               WRK-OUT-DESSUBP-FLTM
           MOVE 5537-SAI-IRSUMO-SIT-DESC  TO  WRK-INP-SITU1-FLTM
                                              WRK-OUT-SITU1-FLTM

           IF  5537-SAI-CCONTR-CONVE-DESC NOT EQUAL ZEROS
               IF  5537-SAI-CELMTO-DESC-COML  EQUAL 8
                   MOVE 'CONV GERAL..:'       TO WRK-OUT-SITU2-FLTM
               ELSE
                   MOVE 'CONV CLIENTE:'       TO WRK-OUT-SITU2-FLTM
               END-IF
               MOVE 5537-SAI-CCONTR-CONVE-DESC
                                          TO WRK-OUT-SITU3-FLTM
           ELSE
               IF  5537-SAI-CCONTR-LIM-DESC NOT EQUAL ZEROS
                   MOVE 'CONTR LIMITE:'   TO WRK-OUT-SITU2-FLTM
                   MOVE 5537-SAI-CCONTR-LIM-DESC
                                          TO WRK-OUT-SITU3-FLTM
               END-IF
           END-IF.

           PERFORM 1111-ACESSAR-DCOM4266

           MOVE 5537-SAI-DANO-BASE
                                  TO  WRK-OUT-NUM1CONS-FLTM.
           MOVE 5537-SAI-CDEPDC
                                  TO  WRK-OUT-NUM2CONS-FLTM.
           MOVE 5537-SAI-CSEQ-STUDO
                                  TO  WRK-OUT-NUM3CONS-FLTM.
           MOVE 5537-SAI-VOPER-DESC-APURA
                                  TO  WRK-OUT-VLROPER-FLTM.
           MOVE 5537-SAI-VTAC-OPER-DESC
                                  TO  WRK-OUT-VLRTARO-FLTM.

           MOVE 5537-SAI-VTARIF-REG-TITLO
                                  TO  WRK-OUT-VLRTART-FLTM.
           MOVE 5537-SAI-DVCTO-FNAL-APURA
                                  TO  WRK-OUT-DVCTFIM-FLTM.

           INSPECT WRK-OUT-DVCTFIM-FLTM REPLACING ALL '.' BY '/'.

           MOVE 5537-SAI-DINIC-OPER-DESC
                                  TO  WRK-OUT-DOPERAC-FLTM
           INSPECT WRK-OUT-DOPERAC-FLTM REPLACING ALL '.' BY '/'

           MOVE 5537-SAI-VJURO-OPER-DESC
                                  TO  WRK-OUT-VLRJUROS-FLTM.
           MOVE 5537-SAI-TMED-PONDE-OPER
                                  TO  WRK-OUT-PRZMEDIO-FLTM.
           MOVE 5537-SAI-VIOF-OPER-DESC
                                  TO  WRK-OUT-VLRIOF-FLTM.
           MOVE 5537-SAI-QTITLO-CHEQ-PCELA
                                  TO  WRK-OUT-QTDTIT-FLTM.
           MOVE 5537-SAI-VLIQ-OPER-DESC
                                  TO  WRK-OUT-VLRLIQ-FLTM.
           MOVE 5537-SAI-CMEIO-ENTRD-DESC
                                  TO  WRK-OUT-CODMEIO-FLTM.
           MOVE 5537-SAI-IRSUMO-MEIO-ENTRD
                                  TO  WRK-OUT-MEIOENT-FLTM.
           MOVE 5537-SAI-CTPO-DESC-COML
                                  TO  WRK-OUT-TPDES-FLTM.
           MOVE 5537-SAI-ITPO-GARNT
                                  TO  WRK-OUT-GARANTIA-FLTM.
           MOVE 5537-SAI-CCART-LIM-CREDT
                                  TO  WRK-OUT-CARTCONS-FLTM.
           MOVE 5537-SAI-CCONTR-CORP-OPER
                                  TO  WRK-OUT-CONTRC-FLTM-N.

           MOVE 5537-SAI-IRSUMO-TPO-TX-JURO
                                  TO  WRK-OUT-TPTAXA-FLTM.

           COMPUTE WRK-VLR-S      = 5537-SAI-VOPER-DESC-APURA
                                  - 5537-SAI-VTAC-OPER-DESC
                                  - 5537-SAI-VTARIF-REG-TITLO
                                  - 5537-SAI-VJURO-OPER-DESC
                                  - 5537-SAI-VIOF-OPER-DESC.

           IF  WRK-VLR-S          LESS ZEROS
               MOVE 5537-SAI-VLIQ-OPER-DESC TO WRK-VLR-S
               COMPUTE WRK-VLR-S            = (WRK-VLR-S * (- 1))
               MOVE WRK-VLR-S               TO WRK-OUT-VLRLIQ-FLTM
           END-IF.

           IF  COMU-MENSAGEM-FLTM           EQUAL SPACES
               MOVE 'CONSULTA EFETUADA COM SUCESSO. TECLE <PF11> PARA CO
      -             'NTINUAR'          TO  WRK-OUT-MENSA-FLTM
           ELSE
               MOVE COMU-MENSAGEM-FLTM TO  WRK-OUT-MENSA-FLTM
           END-IF.

BRQ001     PERFORM 1600-OBTER-INFO-LIBRC-MIDIA.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA OBTEM A PERIODICIDADE DA TAXA                     *
      *----------------------------------------------------------------*
       1111-ACESSAR-DCOM4266           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4266E-ENTRADA
                      4266S-SAIDA

           MOVE 'DCOM4266'              TO  WRK-MODULO.

           MOVE  WRK-COD-USER           TO  4266E-CFUNC-BDSCO.
           MOVE  LNK-IO-LTERM           TO  4266E-CTERM.
           MOVE  WRK-OUT-DANO-OPER-FLTM
                                        TO  4266E-DANO-OPER-DESC.
           MOVE  WRK-OUT-NSEQ-OPER-FLTM
                                        TO  4266E-NSEQ-OPER-DESC.
           MOVE  WRK-OUT-CNPJCPF-FLTM(1:3)
                                        TO  4266E-CCNPJ-CPF(1:3).
           MOVE  WRK-OUT-CNPJCPF-FLTM(5:3)
                                        TO  4266E-CCNPJ-CPF(4:3).
           MOVE  WRK-OUT-CNPJCPF-FLTM(9:3)
                                        TO  4266E-CCNPJ-CPF(7:3).
           MOVE  WRK-OUT-CNPJCPF-FLTM(13:4)
                                        TO  4266E-CFLIAL-CNPJ.

           CALL  WRK-MODULO          USING  4266E-ENTRADA
                                            4266S-SAIDA
                                            ERRO-AREA
                                            WRK-SQLCA.

           EVALUATE 4266S-COD-RETORNO

              WHEN '0000'
                   MOVE 4266S-IRSUMO-TPO-TX-JURO   TO
                        WRK-OUT-TPTAXA-FLTM
                   IF  4266S-CINDCD-FREQ-TX  EQUAL 'AA'
                       MOVE 'A.A.'             TO   WRK-OUT-TXDESC-FLTM
                       MOVE 5537-SAI-PTX-JURO-ANO
                                               TO   WRK-OUT-TXJUROS-FLTM
                   ELSE
                       MOVE 'A.M.'             TO   WRK-OUT-TXDESC-FLTM
                       MOVE 5537-SAI-PTX-JURO-MES
                                               TO   WRK-OUT-TXJUROS-FLTM
                   END-IF

              WHEN '0099'
                    MOVE   'DB2'               TO   ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE 'A.M.'                TO  WRK-OUT-TXDESC-FLTM
                    MOVE 5537-SAI-PTX-JURO-MES
                                               TO  WRK-OUT-TXJUROS-FLTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1131-BUSCAR-DESC-GARANTIA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ENTRADA-6425-ROTEADOR
                      SAIDA-6425-ROTEADOR.

           MOVE 23                     TO ROTENT-6425-CGARAN.
           MOVE 'DCOM6425'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ENTRADA-6425-ROTEADOR
                                             SAIDA-6425-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  ROTSAI-6425-COD-RETORNO
               WHEN '0000'
                     MOVE ROTSAI-6425-IGARAN  TO WRK-OUT-GARANTIA-FLTM

              WHEN  '0099'
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

      *----------------------------------------------------------------*
       1131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMFLTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMFLTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-FLTM.

           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-FLTM     EQUAL 'H' OR
                   (WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-FLTM TO WRK-OUT-MENSA-FLTM

               WHEN WRK-INP-PFK-FLTM     EQUAL '3' OR
                   (WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FLTM     EQUAL 'X' OR
                   (WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-FLTM     EQUAL 'Y' OR
                   (WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL 'PFK09')
                    PERFORM 1242-TRATAR-PARCELA

               WHEN WRK-INP-PFK-FLTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-FLTM     EQUAL 'C' OR
                   (WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL 'PFK11')
                    PERFORM 1241-TRATAR-CONTINUACAO

PROC01         WHEN WRK-INP-PFK-FLTM     EQUAL 'D' OR
PROC01             (WRK-INP-PFK-FLTM     EQUAL '.' AND
PROC01              WRK-INP-COMANDO-FLTM EQUAL 'PFK12')
PROC01              PERFORM 1400-CONSULTAR-CET
PROC01
               WHEN WRK-INP-PFK-FLTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FLTM EQUAL  SPACES OR LOW-VALUES
                        MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11
      -                      '> PARA CONTINUAR'   TO WRK-OUT-MENSA-FLTM

               WHEN OTHER
                    MOVE SG-MENSAGEM(38) TO WRK-OUT-MENSA-FLTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-NOMECLI-FLTM   TO WRK-OUT-NOMECLI-FLTM
           MOVE WRK-INP-TPOPCAO-FLTM   TO WRK-OUT-TPOPCAO-FLTM
           MOVE WRK-INP-CNPJCPF-FLTM   TO WRK-OUT-CNPJCPF-FLTM
           MOVE WRK-INP-DADOSBC-FLTM   TO WRK-OUT-DADOSBC-FLTM
           MOVE WRK-INP-CODPROD-FLTM   TO WRK-OUT-CODPROD-FLTM-X
           MOVE WRK-INP-DESPROD-FLTM   TO WRK-OUT-DESPROD-FLTM
           MOVE WRK-INP-CODSUBP-FLTM   TO WRK-OUT-CODSUBP-FLTM-X
           MOVE WRK-INP-DESSUBP-FLTM   TO WRK-OUT-DESSUBP-FLTM
           MOVE WRK-INP-OPERACAO-FLTM-X TO WRK-OUT-OPERACAO-FLTM-X
           MOVE WRK-INP-SITU1-FLTM     TO WRK-OUT-SITU1-FLTM
           MOVE WRK-INP-SITU2-FLTM     TO WRK-OUT-SITU2-FLTM
           MOVE WRK-INP-SITU3-FLTM     TO WRK-OUT-SITU3-FLTM
           MOVE WRK-INP-SITU4-FLTM     TO WRK-OUT-SITU4-FLTM
           MOVE WRK-INP-SITU5-FLTM     TO WRK-OUT-SITU5-FLTM
           MOVE WRK-INP-TPDES-FLTM     TO WRK-OUT-TPDES-FLTM
           MOVE WRK-INP-TIMEST-FLTM    TO WRK-OUT-TIMEST-FLTM
           MOVE WRK-INP-PGMANT-FLTM    TO WRK-OUT-PGMANT-FLTM
           MOVE WRK-INP-CARTCONS-FLTM  TO WRK-OUT-CARTCONS-FLTM-X.
           MOVE WRK-INP-NUM1CONS-FLTM  TO WRK-OUT-NUM1CONS-FLTM-X.
           MOVE WRK-INP-NUM2CONS-FLTM  TO WRK-OUT-NUM2CONS-FLTM-X.
           MOVE WRK-INP-NUM3CONS-FLTM  TO WRK-OUT-NUM3CONS-FLTM-X.
           MOVE WRK-INP-VLROPER-FLTM   TO WRK-OUT-VLROPER-FLTM-X.
           MOVE WRK-INP-TPTAXA-FLTM    TO WRK-OUT-TPTAXA-FLTM
           MOVE WRK-INP-VLRTARO-FLTM   TO WRK-OUT-VLRTARO-FLTM-X.
           MOVE WRK-INP-TXJUROS-FLTM   TO WRK-OUT-TXJUROS-FLTM-X.
           MOVE WRK-INP-TXDESC-FLTM    TO WRK-OUT-TXDESC-FLTM
           MOVE WRK-INP-VLRTART-FLTM   TO WRK-OUT-VLRTART-FLTM-X.
           MOVE WRK-INP-DOPERAC-FLTM   TO WRK-OUT-DOPERAC-FLTM
           MOVE WRK-INP-DVCTFIM-FLTM   TO WRK-OUT-DVCTFIM-FLTM
           MOVE WRK-INP-VLRJUROS-FLTM  TO WRK-OUT-VLRJUROS-FLTM-X.
           MOVE WRK-INP-PRZMEDIO-FLTM  TO WRK-OUT-PRZMEDIO-FLTM-X.
           MOVE WRK-INP-VLRIOF-FLTM    TO WRK-OUT-VLRIOF-FLTM-X.
           MOVE WRK-INP-QTDTIT-FLTM    TO WRK-OUT-QTDTIT-FLTM-X.
           MOVE WRK-INP-VLRLIQ-FLTM    TO WRK-OUT-VLRLIQ-FLTM-X.
           MOVE WRK-INP-GARANTIA-FLTM  TO WRK-OUT-GARANTIA-FLTM
           MOVE WRK-INP-CONTRC-FLTM    TO WRK-OUT-CONTRC-FLTM.
           MOVE WRK-INP-CODMEIO-FLTM   TO WRK-OUT-CODMEIO-FLTM.
           MOVE WRK-INP-MEIOENT-FLTM   TO WRK-OUT-MEIOENT-FLTM.
BRQ001     MOVE WRK-INP-LIBMID-FLTM    TO WRK-OUT-LIBMID-FLTM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*
      *
      *    TRATAMENTO PADRAO PARA RETORNO
      *
           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP
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
           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FLTM
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0612'           TO COMU-TRANSACAO-FLTM.
           MOVE LENGTH OF COMU-AREA-IMS
                                     TO COMU-LL-FLTM

           IF  WRK-NAO-ACHOU         EQUAL  'S'
               MOVE 'DEMONSTRATIVO NAO ENCONTRADO PARA CONSULTA'
                                     TO COMU-MENSAGEM-FLTM
           ELSE
               MOVE WRK-OUT-MENSA-FLTM TO COMU-MENSAGEM-FLTM
           END-IF.

           MOVE COMU-AREA-IMS        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0612'          TO WRK-TELA
                                       COMU-TRANSACAO-FLTM.
           MOVE '5'                 TO COMU-PFK-FLTM.
           MOVE COMU-AREA-IMS       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0612'         TO WRK-TELA
                                      COMU-TRANSACAO-FLTM.
           MOVE 'A'                TO COMU-PFK-FLTM.
           MOVE COMU-AREA-IMS      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1242-TRATAR-PARCELA             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FLTM

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           IF  WRK-SITUACAO                EQUAL 2
               MOVE 'SITUACAO DA OPERACAO NAO PERMITE CONSULTA DE PARCEL
      -             'A'                    TO WRK-OUT-MENSA-FLTM
               GO TO 1241-99-FIM
           END-IF.

           MOVE WRK-INP-NOMECLI-FLTM    TO WRK-NOMECLI
           MOVE WRK-INP-CNPJCPF-FLTM    TO WRK-CNPJCPF
           MOVE WRK-INP-CONTA-FLTM      TO WRK-CONTA
           MOVE WRK-INP-AGENCIA-FLTM    TO WRK-AGENCIA
           MOVE WRK-INP-SITU1-FLTM      TO WRK-SITU1
           MOVE WRK-INP-SITU2-FLTM      TO WRK-SITU2
           MOVE WRK-INP-SITU3-FLTM      TO WRK-SITU3
           MOVE WRK-INP-SITU4-FLTM      TO WRK-SITU4
           MOVE WRK-INP-SITU5-FLTM      TO WRK-SITU5
           MOVE WRK-INP-CODPROD-FLTM    TO WRK-PRODUTO
           MOVE WRK-INP-DESPROD-FLTM    TO WRK-DESC-PRODUTO
           MOVE WRK-INP-CODSUBP-FLTM    TO WRK-SUBPRODUTO
           MOVE WRK-INP-DESSUBP-FLTM    TO WRK-DESC-SPROD
           MOVE WRK-INP-NUM1CONS-FLTM   TO WRK-NUM1CONS
           MOVE WRK-INP-NUM2CONS-FLTM   TO WRK-NUM2CONS
           MOVE WRK-INP-NUM3CONS-FLTM   TO WRK-NUM3CONS
           MOVE WRK-INP-VLRTARO-FLTM    TO WRK-VLRTARO
           MOVE WRK-INP-VLRTART-FLTM    TO WRK-VLRTART
           MOVE WRK-INP-DVCTFIM-FLTM    TO WRK-DVCTFIM
           MOVE WRK-INP-DOPERAC-FLTM    TO WRK-DOPERAC
           MOVE WRK-INP-VLRJUROS-FLTM   TO WRK-VLRJUROS
           MOVE WRK-INP-PRZMEDIO-FLTM   TO WRK-PRZMEDIO
           MOVE WRK-INP-VLRIOF-FLTM     TO WRK-VLRIOF
           MOVE WRK-INP-QTDTIT-FLTM     TO WRK-QTDTIT
           MOVE WRK-INP-VLRLIQ-FLTM     TO WRK-VLRLIQ
           MOVE WRK-INP-CODMEIO-FLTM    TO WRK-CODMEIO
           MOVE WRK-INP-MEIOENT-FLTM    TO WRK-MEIOENT
           MOVE WRK-INP-TPDES-FLTM      TO WRK-TPDES
           MOVE WRK-INP-GARANTIA-FLTM   TO WRK-GARANTIA
           MOVE WRK-INP-CARTCONS-FLTM   TO WRK-CARTCONS
           MOVE WRK-INP-CONTRC-FLTM-N   TO WRK-CONTRC
           MOVE WRK-INP-TPTAXA-FLTM     TO WRK-TPTAXA
           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0612'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO-FLTM
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL-FLTM
      *
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE 'DCOM0629'                 TO WRK-TELA.

      *----------------------------------------------------------------*
       1241-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1241-TRATAR-CONTINUACAO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMEST-FLTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FLTM

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-INP-NOMECLI-FLTM    TO WRK-NOMECLI
           MOVE WRK-INP-CNPJCPF-FLTM    TO WRK-CNPJCPF
           MOVE WRK-INP-AGENCIA-FLTM    TO WRK-AGENCIA
           MOVE WRK-INP-CONTA-FLTM      TO WRK-CONTA
           MOVE WRK-INP-SITU1-FLTM      TO WRK-SITU1
           MOVE WRK-INP-SITU2-FLTM      TO WRK-SITU2
           MOVE WRK-INP-SITU3-FLTM      TO WRK-SITU3
           MOVE WRK-INP-SITU4-FLTM      TO WRK-SITU4
           MOVE WRK-INP-SITU5-FLTM      TO WRK-SITU5
           MOVE WRK-INP-NUM1CONS-FLTM   TO WRK-NUM1CONS
           MOVE WRK-INP-NUM2CONS-FLTM   TO WRK-NUM2CONS
           MOVE WRK-INP-NUM3CONS-FLTM   TO WRK-NUM3CONS
           MOVE WRK-INP-CODPROD-FLTM    TO WRK-PRODUTO
           MOVE WRK-INP-DESPROD-FLTM    TO WRK-DESC-PRODUTO
           MOVE WRK-INP-CODSUBP-FLTM    TO WRK-SUBPRODUTO
           MOVE WRK-INP-DESSUBP-FLTM    TO WRK-DESC-SPROD
           MOVE WRK-INP-VLRTARO-FLTM    TO WRK-VLRTARO
           MOVE WRK-INP-VLRTART-FLTM    TO WRK-VLRTART
           MOVE WRK-INP-DVCTFIM-FLTM    TO WRK-DVCTFIM
           MOVE WRK-INP-DOPERAC-FLTM    TO WRK-DOPERAC
           MOVE WRK-INP-VLRJUROS-FLTM   TO WRK-VLRJUROS
           MOVE WRK-INP-PRZMEDIO-FLTM   TO WRK-PRZMEDIO
           MOVE WRK-INP-VLRIOF-FLTM     TO WRK-VLRIOF
           MOVE WRK-INP-QTDTIT-FLTM     TO WRK-QTDTIT
           MOVE WRK-INP-VLRLIQ-FLTM     TO WRK-VLRLIQ
           MOVE WRK-INP-CODMEIO-FLTM    TO WRK-CODMEIO
           MOVE WRK-INP-MEIOENT-FLTM    TO WRK-MEIOENT
           MOVE WRK-INP-TPDES-FLTM      TO WRK-TPDES
           MOVE WRK-INP-GARANTIA-FLTM   TO WRK-GARANTIA
           MOVE WRK-INP-CARTCONS-FLTM   TO WRK-CARTCONS
           MOVE WRK-INP-CONTRC-FLTM-N   TO WRK-CONTRC
           MOVE WRK-INP-TPTAXA-FLTM     TO WRK-TPTAXA

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15

               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0612'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO-FLTM
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL-FLTM
      *
           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE 'DCOM0613'                 TO WRK-TELA.

      *----------------------------------------------------------------*
       1241-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PROC01*----------------------------------------------------------------*
PROC01*  ESSA ROTINA EFETUA A CONSULTA DO CET                          *
PROC01*----------------------------------------------------------------*
PROC01 1400-CONSULTAR-CET              SECTION.
PROC01*----------------------------------------------------------------*
PROC01
PROC01     INITIALIZE COMU-AREA-IMS.
PROC01
PROC01     MOVE LENGTH                 OF COMU-AREA-IMS
PROC01                                 TO COMU-LL-FLTM.
PROC01     MOVE WRK-INP-TIMEST-FLTM    TO COMU-TIMESTAMP-FLTM
PROC01     MOVE WRK-INP-DANO-OPER-FLTM TO WRK-COMU-DANO-FLTM.
PROC01     MOVE WRK-INP-NSEQ-OPER-FLTM TO WRK-COMU-NSEQ-FLTM.
PROC01     MOVE 'DCOM3924'             TO WRK-TELA.
PROC01     MOVE 'DCOM0612'             TO COMU-TRANSACAO-FLTM.
PROC01     MOVE WRK-CHNG               TO WRK-FUNCAO.
PROC01     MOVE COMU-AREA-IMS          TO WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 1400-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-FLTM
                                       TO  WRK-660-LL-MENSAGEM-FLTM
                                           WRK-OUT-LL-FLTM

           MOVE LENGTH OF WRK-660-DCOMFLTM
                                       TO  WRK-660-LL-AREA-FLTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-FLTM
                                             WRK-660-DCOMFLTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FLTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001*     OBTER PARAMETRO DE LIBERACAO NAS MIDIAS
BRQ001*----------------------------------------------------------------*
BRQ001 1600-OBTER-INFO-LIBRC-MIDIA     SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     INITIALIZE WRK-5490-ENTRADA
BRQ001                WRK-5490-SAIDA
BRQ001                ERRO-AREA
BRQ001                WRK-SQLCA.
BRQ001
BRQ001     MOVE WRK-DANO-OPER-DESC     TO WRK-E5490-DANO-OPER.
BRQ001     MOVE WRK-NSEQ-OPER-DESC     TO WRK-E5490-NSEQ-OPER.
BRQ001     MOVE 'CO'                   TO WRK-E5490-FUNCAO.
BRQ001     MOVE 'DCOM5490'             TO WRK-MODULO.
BRQ001
BRQ001     CALL  WRK-MODULO            USING WRK-5490-ENTRADA
BRQ001                                       WRK-5490-SAIDA
BRQ001                                       ERRO-AREA
BRQ001                                       WRK-SQLCA.
BRQ001
BRQ001     IF  WRK-S5490-COD-RETORNO   EQUAL 99
BRQ001         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BRQ001         PERFORM 9999-PROCESSAR-ROTINA-ERRO
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-S5490-LIBRC-MIDIA   EQUAL 'S'
BRQ001         MOVE 'SIM'              TO WRK-OUT-LIBMID-FLTM
BRQ001     ELSE
BRQ001         MOVE 'NAO'              TO WRK-OUT-LIBMID-FLTM
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 1600-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
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
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0612'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0612'         TO ERR-PGM
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
