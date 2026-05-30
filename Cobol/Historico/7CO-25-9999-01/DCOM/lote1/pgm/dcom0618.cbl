      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0618.
       AUTHOR.     FABRICA ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0618                                    *
      *    PROGRAMADOR.:   FABRICA - ALTRAN.                           *
      *    ANALISTA....:   EMERSON ARTUR FRANCO - ALTRAN.              *
      *    DATA........:   23/02/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR HISTORICO - DEMOSTRATIVO          *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMFRTM - CONSULTAR HISTORICO - DEMONSTRATIVO              *
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
           '*** DCOM0618 - INICIO DA AREA DE WORKING ***'.
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
      *
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
           '*** AREA DA TELA DCOMFRTM  - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-FRTM.
           03  WRK-INP-LL-FRTM              PIC 9(04) COMP.
           03  WRK-INP-ZZ-FRTM              PIC 9(04) COMP.
           03  WRK-INP-TRANCODE-FRTM        PIC X(08).
           03  FILLER                       PIC X(06).
           03  WRK-INP-PFK-FRTM             PIC X(01).
           03  WRK-INP-SENHAS-FRTM          PIC X(37).
           03  WRK-INP-COMANDO-FRTM         PIC X(68).
           03  WRK-INP-NOMECLI-FRTM         PIC X(40).
           03  WRK-INP-TPOPCAO-FRTM         PIC 9(01).
           03  WRK-INP-CNPJCPF-FRTM         PIC X(19).
           03  WRK-INP-DADOSBC-FRTM.
               05  WRK-INP-BANCO-FRTM       PIC 9(03).
               05  FILLER                   PIC X(01) VALUE '/'.
               05  WRK-INP-AGENCIA-FRTM     PIC 9(05).
               05  FILLER                   PIC X(01) VALUE '/'.
               05  WRK-INP-CONTA-FRTM       PIC 9(13).
           03  WRK-INP-CODPROD-FRTM         PIC X(03).
           03  WRK-INP-DESPROD-FRTM         PIC X(12).
           03  WRK-INP-CODSUBP-FRTM         PIC X(03).
           03  WRK-INP-DESSUBP-FRTM         PIC X(12).
           03  WRK-INP-OPERACAO-FRTM-X.
               05  WRK-INP-DANO-OPER-FRTM   PIC 9(04).
               05  WRK-INP-NSEQ-OPER-FRTM   PIC 9(09).
           03  WRK-INP-SITU1-FRTM           PIC X(15).
           03  WRK-INP-SITU2-FRTM           PIC X(13).
           03  WRK-INP-SITU3-FRTM           PIC X(09).
           03  WRK-INP-DTADIT-FRTM          PIC X(10).
           03  WRK-INP-SITU4-FRTM           PIC X(13).
           03  WRK-INP-SITU5-FRTM           PIC X(09).
           03  WRK-INP-TPDES-FRTM           PIC X(03).
           03  WRK-INP-TIMEST-FRTM          PIC X(26).
           03  WRK-INP-PGMANT-FRTM          PIC X(08).
           03  WRK-INP-CARTCONS-FRTM        PIC X(03).
           03  WRK-INP-NUM1CONS-FRTM        PIC X(04).
           03  WRK-INP-NUM2CONS-FRTM        PIC X(05).
           03  WRK-INP-NUM3CONS-FRTM        PIC X(09).
           03  WRK-INP-DOPERAC-FRTM         PIC X(10).
           03  WRK-INP-VLROPER-FRTM         PIC X(14).
           03  WRK-INP-TPTAXA-FRTM          PIC X(15).
           03  WRK-INP-VLRTARO-FRTM         PIC X(14).
           03  WRK-INP-TXJUROS-FRTM         PIC X(09).
           03  WRK-INP-TXDESC-FRTM          PIC X(04).
           03  WRK-INP-VLRTART-FRTM         PIC X(14).
           03  WRK-INP-DVCTFIM-FRTM         PIC X(10).
           03  WRK-INP-VLRJUROS-FRTM        PIC X(14).
           03  WRK-INP-PRZMEDIO-FRTM        PIC X(03).
           03  WRK-INP-VLRIOF-FRTM          PIC X(14).
           03  WRK-INP-QTDTIT-FRTM          PIC X(06).
           03  WRK-INP-VLRLIQ-FRTM          PIC X(15).
           03  WRK-INP-GARANTIA-FRTM        PIC X(20).
           03  WRK-INP-CONTRC-FRTM.
               05  WRK-INP-CONTRC-FRTM-N    PIC 9(07).
           03  WRK-INP-CODMEIO-FRTM         PIC X(03).
           03  WRK-INP-MEIOENT-FRTM         PIC X(15).
BRQ001     03  WRK-INP-LIBMID-FRTM          PIC X(03)      VALUE SPACES.
           03  WRK-INP-MENSA-FRTM           PIC X(79).

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFRTM  - OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT-FRTM.
           03  WRK-OUT-LL-FRTM              PIC 9(04) COMP VALUE 0000.
           03  WRK-OUT-ZZ-FRTM              PIC 9(04) COMP VALUE ZEROS.
           03  WRK-OUT-SENHAS-FRTM          PIC X(37).
           03  WRK-OUT-COMANDO-FRTM         PIC X(68).
           03  WRK-OUT-DADOS-FRTM.
               05  WRK-OUT-NOMECLI-FRTM         PIC X(40).
               05  WRK-OUT-TPOPCAO-FRTM         PIC 9(01).
               05  WRK-OUT-CNPJCPF-FRTM         PIC X(19).
               05  WRK-OUT-DADOSBC-FRTM.
                   07  WRK-OUT-BANCO-FRTM       PIC 9(03).
                   07  WRK-OUT-BARRA-1-FRTM     PIC X(01) VALUE '/'.
                   07  WRK-OUT-AGENCIA-FRTM     PIC 9(05).
                   07  WRK-OUT-BARRA-2-FRTM     PIC X(01) VALUE '/'.
                   07  WRK-OUT-CONTA-FRTM       PIC 9(13).
               05  WRK-OUT-CODPROD-FRTM-X.
                   07  WRK-OUT-CODPROD-FRTM     PIC 9(03).
               05  WRK-OUT-DESPROD-FRTM         PIC X(12).
               05  WRK-OUT-CODSUBP-FRTM-X.
                   07  WRK-OUT-CODSUBP-FRTM     PIC 9(03).
               05  WRK-OUT-DESSUBP-FRTM         PIC X(12).
               05  WRK-OUT-OPERACAO-FRTM-X.
                   07  WRK-OUT-DANO-OPER-FRTM   PIC 9(04).
                   07  WRK-OUT-NSEQ-OPER-FRTM   PIC 9(09).
               05  WRK-OUT-SITU1-FRTM           PIC X(15).
               05  WRK-OUT-SITU2-FRTM           PIC X(13).
               05  WRK-OUT-SITU3-FRTM           PIC X(09).
               05  WRK-OUT-DTADIT-FRTM          PIC X(10).
               05  WRK-OUT-SITU4-FRTM           PIC X(13).
               05  WRK-OUT-SITU5-FRTM           PIC X(09).
               05  WRK-OUT-TPDES-FRTM-X.
                   10  WRK-OUT-TPDES-FRTM       PIC 9(03).
               05  WRK-OUT-TIMEST-FRTM          PIC X(26).
               05  WRK-OUT-PGMANT-FRTM          PIC X(08).
               05  WRK-OUT-CARTCONS-FRTM-X.
                   07  WRK-OUT-CARTCONS-FRTM    PIC X(03).
               05  WRK-OUT-NUM1CONS-FRTM-X.
                   07  WRK-OUT-NUM1CONS-FRTM    PIC 9(04).
               05  WRK-OUT-NUM2CONS-FRTM-X.
                   07  WRK-OUT-NUM2CONS-FRTM    PIC 9(05).
               05  WRK-OUT-NUM3CONS-FRTM-X.
                   07  WRK-OUT-NUM3CONS-FRTM    PIC 9(09).
               05  WRK-OUT-DOPERAC-FRTM         PIC X(10).
               05  WRK-OUT-VLROPER-FRTM-X.
                   07  WRK-OUT-VLROPER-FRTM     PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-TPTAXA-FRTM          PIC X(15).
               05  WRK-OUT-VLRTARO-FRTM-X.
                   07  WRK-OUT-VLRTARO-FRTM     PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-TXJUROS-FRTM-X.
                   07  WRK-OUT-TXJUROS-FRTM     PIC ZZ9,99999.
               05  WRK-OUT-TXDESC-FRTM          PIC X(04).
               05  WRK-OUT-VLRTART-FRTM-X.
                   07  WRK-OUT-VLRTART-FRTM     PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-DVCTFIM-FRTM         PIC X(10).
               05  WRK-OUT-VLRJUROS-FRTM-X.
                   07  WRK-OUT-VLRJUROS-FRTM    PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-PRZMEDIO-FRTM-X.
                   07  WRK-OUT-PRZMEDIO-FRTM    PIC 9(03).
               05  WRK-OUT-VLRIOF-FRTM-X.
                   07  WRK-OUT-VLRIOF-FRTM      PIC ZZZ.ZZZ.ZZ9,99.
               05  WRK-OUT-QTDTIT-FRTM-X.
                   07  WRK-OUT-QTDTIT-FRTM      PIC 999999.
               05  WRK-OUT-VLRLIQ-FRTM-X.
                   07  WRK-OUT-VLRLIQ-FRTM      PIC ----.---.--9,99.
               05  WRK-OUT-GARANTIA-FRTM        PIC X(20).
               05  WRK-OUT-CONTRC-FRTM.
                   07  WRK-OUT-CONTRC-FRTM-N    PIC 9(07).
               05  WRK-OUT-CODMEIO-FRTM-X.
                   07  WRK-OUT-CODMEIO-FRTM     PIC 9(03).
               05  WRK-OUT-MEIOENT-FRTM         PIC X(15).
BRQ001         05  WRK-OUT-LIBMID-FRTM          PIC X(03)  VALUE SPACES.
               05  WRK-OUT-MENSA-FRTM           PIC X(79).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMFRTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMFRTM.
           03  WRK-660-LL-AREA-FRTM         PIC 9(04) COMP VALUE 0000.
           03  WRK-660-LL-MENSAGEM-FRTM     PIC 9(04) COMP VALUE 0000.
           03  WRK-660-SENHAS-FRTM          PIC 9(04) COMP VALUE 0037.
           03  WRK-660-COMANDO-FRTM         PIC 9(04) COMP VALUE 0068.
           03  WRK-660-NOMECLI-FRTM         PIC 9(04) COMP VALUE 0040.
           03  WRK-660-TPOPCAO-FRTM         PIC 9(04) COMP VALUE 0001.
           03  WRK-660-CNPJCPF-FRTM         PIC 9(04) COMP VALUE 0019.
           03  WRK-660-DADOSBC-FRTM         PIC 9(04) COMP VALUE 0023.
           03  WRK-660-CODPROD-FRTM         PIC 9(04) COMP VALUE 0003.
           03  WRK-660-DESPROD-FRTM         PIC 9(04) COMP VALUE 0012.
           03  WRK-660-CODSUBP-FRTM         PIC 9(04) COMP VALUE 0003.
           03  WRK-660-DESSUBP-FRTM         PIC 9(04) COMP VALUE 0012.
           03  WRK-660-OPERACAO-FRTM        PIC 9(04) COMP VALUE 0013.
           03  WRK-660-SITU1-FRTM           PIC 9(04) COMP VALUE 0015.
           03  WRK-660-SITU2-FRTM           PIC 9(04) COMP VALUE 0013.
           03  WRK-660-SITU3-FRTM           PIC 9(04) COMP VALUE 0009.
           03  WRK-660-DTADIT-FRTM          PIC 9(04) COMP VALUE 0010.
           03  WRK-660-SITU4-FRTM           PIC 9(04) COMP VALUE 0013.
           03  WRK-660-SITU5-FRTM           PIC 9(04) COMP VALUE 0009.
           03  WRK-660-TPDES-FRTM           PIC 9(04) COMP VALUE 0003.
           03  WRK-660-TIMEST-FRTM          PIC 9(04) COMP VALUE 0026.
           03  WRK-660-PGMANT-FRTM          PIC 9(04) COMP VALUE 0008.
           03  WRK-660-CARTCONS-FRTM        PIC 9(04) COMP VALUE 0003.
           03  WRK-660-NUM1CONS-FRTM        PIC 9(04) COMP VALUE 0004.
           03  WRK-660-NUM2CONS-FRTM        PIC 9(04) COMP VALUE 0005.
           03  WRK-660-NUM3CONS-FRTM        PIC 9(04) COMP VALUE 0009.
           03  WRK-660-DOPERAC-FRTM         PIC 9(04) COMP VALUE 0010.
           03  WRK-660-VLROPER-FRTM         PIC 9(04) COMP VALUE 0014.
           03  WRK-660-TPTAXA-FRTM          PIC 9(04) COMP VALUE 0015.
           03  WRK-660-VLRTARO-FRTM         PIC 9(04) COMP VALUE 0014.
           03  WRK-660-TXJUROS-FRTM         PIC 9(04) COMP VALUE 0009.
           03  WRK-660-TXDESC-FRTM          PIC 9(04) COMP VALUE 0004.
           03  WRK-660-VLRTART-FRTM         PIC 9(04) COMP VALUE 0014.
           03  WRK-660-DVCTFIM-FRTM         PIC 9(04) COMP VALUE 0010.
           03  WRK-660-VLRJUROS-FRTM        PIC 9(04) COMP VALUE 0014.
           03  WRK-660-PRZMEDIO-FRTM        PIC 9(04) COMP VALUE 0003.
           03  WRK-660-VLRIOF-FRTM          PIC 9(04) COMP VALUE 0014.
           03  WRK-660-QTDTIT-FRTM          PIC 9(04) COMP VALUE 0006.
           03  WRK-660-VLRLIQ-FRTM          PIC 9(04) COMP VALUE 0015.
           03  WRK-660-GARANTIA-FRTM        PIC 9(04) COMP VALUE 0020.
           03  WRK-660-CONTRC-FRTM          PIC 9(04) COMP VALUE 0007.
           03  WRK-660-CODMEIO-FRTM         PIC 9(04) COMP VALUE 0003.
           03  WRK-660-MEIOENT-FRTM         PIC 9(04) COMP VALUE 0015.
BRQ001     03  WRK-660-LIBMID-FRTM          PIC 9(04) COMP VALUE 0003.
           03  WRK-660-MENSA-FRTM           PIC 9(04) COMP VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMAS CHAMADORES           ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-IMS.
           05  COMU-LL-FRTM              PIC S9(004) COMP  VALUE +406.
           05  COMU-ZZ-FRTM              PIC S9(004) COMP  VALUE ZEROS.
           05  COMU-TRANCODE-FRTM.
               10  COMU-TRANSACAO-FRTM   PIC  X(008)       VALUE SPACES.
               10  FILLER                PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-FRTM         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-FRTM          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-FRTM         PIC  X(068)       VALUE SPACES.
           05  COMU-TIMESTAMP-FRTM       PIC  X(026)       VALUE SPACES.
           05  COMU-MENSAGEM-FRTM        PIC  X(079)       VALUE SPACES.
PROC01     05  WRK-COMU-DANO-FRTM        PIC  9(004)       VALUE ZEROS.
PROC01     05  WRK-COMU-NSEQ-FRTM        PIC  9(009)       VALUE ZEROS.
PROC01     05  WRK-COMU-TIME-AUX-FRTM    PIC  X(026)       VALUE SPACES.

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
           '*** DCOM0618 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES  TO  WRK-OUT-DADOS-FRTM
                            WRK-OUT-SENHAS-FRTM
                            WRK-OUT-COMANDO-FRTM
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

           MOVE 'DCOMFRTM'  TO  WRK-TELA.

            IF  WRK-TRANSACAO NOT EQUAL 'DCOM0618'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMFRTM
           END-IF.

           IF  WRK-TELA EQUAL 'DCOMFRTM'
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

           MOVE COMU-TIMESTAMP-FRTM        TO WRK-OUT-TIMEST-FRTM
                                              WRK-INP-TIMEST-FRTM
                                              WRK-ENT-TIMESTAMP

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-OPCAO                  TO WRK-INP-TPOPCAO-FRTM
                                              WRK-OUT-TPOPCAO-FRTM

           INITIALIZE 5537-ENTRADA-ROTEADOR

           MOVE 'H'                     TO  5537-ENT-FLAG
           MOVE WRK-COD-USER            TO  5537-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM            TO  5537-ENT-CTERM
           MOVE WRK-DANO-OPER-DESC      TO  5537-ENT-DANO-OPER-DESC
                                            WRK-INP-DANO-OPER-FRTM
                                            WRK-OUT-DANO-OPER-FRTM
           MOVE WRK-NSEQ-OPER-DESC      TO  5537-ENT-NSEQ-OPER-DESC
                                            WRK-INP-NSEQ-OPER-FRTM
                                            WRK-OUT-NSEQ-OPER-FRTM
           MOVE WRK-TIMESTAMP           TO  5537-ENT-HULT-ATULZ

           MOVE 'DCOM5537'              TO  WRK-MODULO
           CALL WRK-MODULO    USING     5537-ENTRADA-ROTEADOR
                                        5537-SAIDA-ROTEADOR
                                        ERRO-AREA
                                        WRK-SQLCA.

           EVALUATE 5537-SAI-COD-RETORNO

               WHEN '0000'
                    PERFORM 1110-MONTAR-TELA

               WHEN '0003'
               WHEN '0100'
                    CONTINUE

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    MOVE  5537-SAI-MSG-RETORNO  TO  ERR-TEXTO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15
                        OR WRK-CHAMADOR(IND-1) EQUAL SPACES
               IF WRK-CHAMADOR(IND-1)          EQUAL 'DCOM0618'
                  MOVE SPACES                  TO WRK-CHAMADOR(IND-1)
                  MOVE ZEROS                   TO WRK-PAGINA(IND-1)
                  MOVE 99                      TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-INP-TIMEST-FRTM        TO WRK-ENT-TIMESTAMP
           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995 .

           INSPECT WRK-DATA-ADITAMENTO REPLACING ALL '.' BY '/'.

           IF  WRK-DATA-ADITAMENTO        EQUAL '01.01.0001' OR
                                                '01/01/0001'
               MOVE SPACES                TO WRK-INP-DTADIT-FRTM
                                             WRK-OUT-DTADIT-FRTM
           ELSE
               MOVE WRK-DATA-ADITAMENTO   TO WRK-INP-DTADIT-FRTM
                                             WRK-OUT-DTADIT-FRTM
           END-IF.

           IF  COMU-MENSAGEM-FRTM         EQUAL SPACES
               MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11> PARA CO
      -             'NTINUAR      '       TO  WRK-OUT-MENSA-FRTM
           ELSE
               MOVE COMU-MENSAGEM-FRTM    TO  WRK-OUT-MENSA-FRTM
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE 5537-SAI-IPSSOA-DESC-COML TO WRK-INP-NOMECLI-FRTM
                                             WRK-OUT-NOMECLI-FRTM

           STRING 5537-SAI-CCNPJ-CPF(1:3)  '.'
                  5537-SAI-CCNPJ-CPF(4:3)  '.'
                  5537-SAI-CCNPJ-CPF(7:3)  '/'
                  5537-SAI-CFLIAL-CNPJ      '-'
                  5537-SAI-CCTRL-CNPJ-CPF
                                      DELIMITED BY SIZE
                                      INTO  WRK-INP-CNPJCPF-FRTM.
           MOVE WRK-INP-CNPJCPF-FRTM    TO  WRK-OUT-CNPJCPF-FRTM.
           MOVE '/'                     TO  WRK-OUT-BARRA-1-FRTM
                                            WRK-OUT-BARRA-2-FRTM
           MOVE 5537-SAI-CBCO           TO  WRK-INP-BANCO-FRTM
                                            WRK-OUT-BANCO-FRTM
           MOVE 5537-SAI-CAG-BCRIA      TO  WRK-INP-AGENCIA-FRTM
                                            WRK-OUT-AGENCIA-FRTM
           MOVE 5537-SAI-CCTA-BCRIA-CLI TO  WRK-INP-CONTA-FRTM
                                            WRK-OUT-CONTA-FRTM
           MOVE 5537-SAI-CPRODT         TO  WRK-INP-CODPROD-FRTM
                                            WRK-OUT-CODPROD-FRTM
           MOVE 5537-SAI-IABREV-PRODT   TO  WRK-INP-DESPROD-FRTM
                                            WRK-OUT-DESPROD-FRTM
           MOVE 5537-SAI-CSPROD-DESC-COML TO  WRK-INP-CODSUBP-FRTM
                                              WRK-OUT-CODSUBP-FRTM
           MOVE 5537-SAI-IRSUMO-SPROD-DESC TO  WRK-INP-DESSUBP-FRTM
                                              WRK-OUT-DESSUBP-FRTM
           MOVE 5537-SAI-IRSUMO-SIT-DESC  TO  WRK-INP-SITU1-FRTM
                                              WRK-OUT-SITU1-FRTM

           IF  5537-SAI-CCONTR-CONVE-DESC NOT EQUAL ZEROS
               IF  5537-SAI-CELMTO-DESC-COML  EQUAL 8
                   MOVE 'CONV GERAL..:'       TO WRK-OUT-SITU2-FRTM
               ELSE
                   MOVE 'CONV CLIENTE:'       TO WRK-OUT-SITU2-FRTM
               END-IF
               MOVE 5537-SAI-CCONTR-CONVE-DESC
                                          TO WRK-OUT-SITU3-FRTM
           ELSE
               IF  5537-SAI-CCONTR-LIM-DESC NOT EQUAL ZEROS
                   MOVE 'CONTR LIMITE:'   TO WRK-OUT-SITU2-FRTM
                   MOVE 5537-SAI-CCONTR-LIM-DESC
                                          TO WRK-OUT-SITU3-FRTM
               END-IF
           END-IF.

           PERFORM 1111-ACESSAR-DCOM4266

           MOVE 5537-SAI-DANO-BASE
                                  TO  WRK-OUT-NUM1CONS-FRTM.
           MOVE 5537-SAI-CDEPDC
                                  TO  WRK-OUT-NUM2CONS-FRTM.
           MOVE 5537-SAI-CSEQ-STUDO
                                  TO  WRK-OUT-NUM3CONS-FRTM.
           MOVE 5537-SAI-VOPER-DESC-APURA
                                  TO  WRK-OUT-VLROPER-FRTM.
           MOVE 5537-SAI-VTAC-OPER-DESC
                                  TO  WRK-OUT-VLRTARO-FRTM.

           MOVE 5537-SAI-VTARIF-REG-TITLO
                                  TO  WRK-OUT-VLRTART-FRTM.
           MOVE 5537-SAI-DVCTO-FNAL-APURA
                                  TO  WRK-OUT-DVCTFIM-FRTM.

           INSPECT WRK-OUT-DVCTFIM-FRTM REPLACING ALL '.' BY '/'.

           MOVE 5537-SAI-DINIC-OPER-DESC
                                  TO  WRK-OUT-DOPERAC-FRTM
           INSPECT WRK-OUT-DOPERAC-FRTM REPLACING ALL '.' BY '/'

           MOVE 5537-SAI-VJURO-OPER-DESC
                                  TO  WRK-OUT-VLRJUROS-FRTM.
           MOVE 5537-SAI-TMED-PONDE-OPER
                                  TO  WRK-OUT-PRZMEDIO-FRTM.
           MOVE 5537-SAI-VIOF-OPER-DESC
                                  TO  WRK-OUT-VLRIOF-FRTM.
           MOVE 5537-SAI-QTITLO-CHEQ-PCELA
                                  TO  WRK-OUT-QTDTIT-FRTM.
           MOVE 5537-SAI-VLIQ-OPER-DESC
                                  TO  WRK-OUT-VLRLIQ-FRTM.
           MOVE 5537-SAI-CMEIO-ENTRD-DESC
                                  TO  WRK-OUT-CODMEIO-FRTM.
           MOVE 5537-SAI-IRSUMO-MEIO-ENTRD
                                  TO  WRK-OUT-MEIOENT-FRTM.
           MOVE 5537-SAI-CTPO-DESC-COML
                                  TO  WRK-OUT-TPDES-FRTM.
           MOVE 5537-SAI-ITPO-GARNT
                                  TO  WRK-OUT-GARANTIA-FRTM.
           MOVE 5537-SAI-CCART-LIM-CREDT
                                  TO  WRK-OUT-CARTCONS-FRTM.
           MOVE 5537-SAI-CCONTR-CORP-OPER
                                  TO  WRK-OUT-CONTRC-FRTM-N.

           MOVE 5537-SAI-IRSUMO-TPO-TX-JURO
                                  TO  WRK-OUT-TPTAXA-FRTM.

           COMPUTE WRK-VLR-S      = 5537-SAI-VOPER-DESC-APURA
                                  - 5537-SAI-VTAC-OPER-DESC
                                  - 5537-SAI-VTARIF-REG-TITLO
                                  - 5537-SAI-VJURO-OPER-DESC
                                  - 5537-SAI-VIOF-OPER-DESC.

           IF  WRK-VLR-S          LESS ZEROS
               MOVE 5537-SAI-VLIQ-OPER-DESC TO WRK-VLR-S
               COMPUTE WRK-VLR-S            = (WRK-VLR-S * (- 1))
               MOVE WRK-VLR-S               TO WRK-OUT-VLRLIQ-FRTM
           END-IF.

BRQ001     PERFORM 1112-OBTER-INFO-LIBRC-MIDIA.

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
           MOVE  WRK-OUT-DANO-OPER-FRTM
                                        TO  4266E-DANO-OPER-DESC.
           MOVE  WRK-OUT-NSEQ-OPER-FRTM
                                        TO  4266E-NSEQ-OPER-DESC.
           MOVE  WRK-OUT-CNPJCPF-FRTM(1:3)
                                        TO  4266E-CCNPJ-CPF(1:3).
           MOVE  WRK-OUT-CNPJCPF-FRTM(5:3)
                                        TO  4266E-CCNPJ-CPF(4:3).
           MOVE  WRK-OUT-CNPJCPF-FRTM(9:3)
                                        TO  4266E-CCNPJ-CPF(7:3).
           MOVE  WRK-OUT-CNPJCPF-FRTM(13:4)
                                        TO  4266E-CFLIAL-CNPJ.

           CALL  WRK-MODULO          USING  4266E-ENTRADA
                                            4266S-SAIDA
                                            ERRO-AREA
                                            WRK-SQLCA.

           EVALUATE 4266S-COD-RETORNO

              WHEN '0000'
                   MOVE 4266S-IRSUMO-TPO-TX-JURO   TO
                        WRK-OUT-TPTAXA-FRTM
                   IF  4266S-CINDCD-FREQ-TX  EQUAL 'AA'
                       MOVE 'A.A.'             TO   WRK-OUT-TXDESC-FRTM
                       MOVE 5537-SAI-PTX-JURO-ANO
                                               TO   WRK-OUT-TXJUROS-FRTM
                   ELSE
                       MOVE 'A.M.'             TO   WRK-OUT-TXDESC-FRTM
                       MOVE 5537-SAI-PTX-JURO-MES
                                               TO   WRK-OUT-TXJUROS-FRTM
                   END-IF

              WHEN '0099'
                    MOVE   'DB2'               TO   ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN  OTHER
                    MOVE 'A.M.'                TO  WRK-OUT-TXDESC-FRTM
                    MOVE 5537-SAI-PTX-JURO-MES
                                               TO  WRK-OUT-TXJUROS-FRTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001*     OBTER PARAMETRO DE LIBERACAO NAS MIDIAS
BRQ001*----------------------------------------------------------------*
BRQ001 1112-OBTER-INFO-LIBRC-MIDIA     SECTION.
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
BRQ001         MOVE 'SIM'              TO WRK-OUT-LIBMID-FRTM
BRQ001     ELSE
BRQ001         MOVE 'NAO'              TO WRK-OUT-LIBMID-FRTM
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 1112-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*

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
                     MOVE ROTSAI-6425-IGARAN  TO WRK-OUT-GARANTIA-FRTM

              WHEN  '0099'
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-EVALUATE.

      *----------------------------------------------------------------*
       1131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMFRTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMFRTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-FRTM.

           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-INP-PFK-FRTM     EQUAL 'H' OR
                   (WRK-INP-PFK-FRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FRTM EQUAL 'PFK01')
                    MOVE WRK-INP-MENSA-FRTM TO WRK-OUT-MENSA-FRTM

               WHEN WRK-INP-PFK-FRTM     EQUAL '3' OR
                   (WRK-INP-PFK-FRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FRTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-FRTM     EQUAL 'X' OR
                   (WRK-INP-PFK-FRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FRTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-FRTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-FRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FRTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-FRTM     EQUAL 'C' OR
                   (WRK-INP-PFK-FRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FRTM EQUAL 'PFK11')
                    PERFORM 1241-TRATAR-CONTINUACAO

PROC01         WHEN WRK-INP-PFK-FRTM     EQUAL 'D' OR
PROC01             (WRK-INP-PFK-FRTM     EQUAL '.' AND
PROC01              WRK-INP-COMANDO-FRTM EQUAL 'PFK12')
PROC01              PERFORM 1400-CONSULTAR-CET
PROC01
               WHEN WRK-INP-PFK-FRTM     EQUAL '.' AND
                    WRK-INP-COMANDO-FRTM EQUAL  SPACES OR LOW-VALUES

                        MOVE 'CONSULTA EFETUADA COM SUCESSO, TECLE <PF11
      -                      '> PARA CONTINUAR'  TO WRK-OUT-MENSA-FRTM

               WHEN OTHER
                    MOVE SG-MENSAGEM(38) TO WRK-OUT-MENSA-FRTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-NOMECLI-FRTM   TO WRK-OUT-NOMECLI-FRTM
           MOVE WRK-INP-TPOPCAO-FRTM   TO WRK-OUT-TPOPCAO-FRTM
           MOVE WRK-INP-CNPJCPF-FRTM   TO WRK-OUT-CNPJCPF-FRTM
           MOVE WRK-INP-DADOSBC-FRTM   TO WRK-OUT-DADOSBC-FRTM
           MOVE WRK-INP-CODPROD-FRTM   TO WRK-OUT-CODPROD-FRTM-X
           MOVE WRK-INP-DESPROD-FRTM   TO WRK-OUT-DESPROD-FRTM
           MOVE WRK-INP-CODSUBP-FRTM   TO WRK-OUT-CODSUBP-FRTM-X
           MOVE WRK-INP-DESSUBP-FRTM   TO WRK-OUT-DESSUBP-FRTM
           MOVE WRK-INP-OPERACAO-FRTM-X TO WRK-OUT-OPERACAO-FRTM-X
           MOVE WRK-INP-SITU1-FRTM     TO WRK-OUT-SITU1-FRTM
           MOVE WRK-INP-SITU2-FRTM     TO WRK-OUT-SITU2-FRTM
           MOVE WRK-INP-SITU3-FRTM     TO WRK-OUT-SITU3-FRTM
           MOVE WRK-INP-SITU4-FRTM     TO WRK-OUT-SITU4-FRTM
           MOVE WRK-INP-SITU5-FRTM     TO WRK-OUT-SITU5-FRTM
           MOVE WRK-INP-TPDES-FRTM     TO WRK-OUT-TPDES-FRTM
           MOVE WRK-INP-DTADIT-FRTM    TO WRK-OUT-DTADIT-FRTM
           MOVE WRK-INP-TIMEST-FRTM    TO WRK-OUT-TIMEST-FRTM
           MOVE WRK-INP-PGMANT-FRTM    TO WRK-OUT-PGMANT-FRTM
           MOVE WRK-INP-CARTCONS-FRTM  TO WRK-OUT-CARTCONS-FRTM-X.
           MOVE WRK-INP-NUM1CONS-FRTM  TO WRK-OUT-NUM1CONS-FRTM-X.
           MOVE WRK-INP-NUM2CONS-FRTM  TO WRK-OUT-NUM2CONS-FRTM-X.
           MOVE WRK-INP-NUM3CONS-FRTM  TO WRK-OUT-NUM3CONS-FRTM-X.
           MOVE WRK-INP-VLROPER-FRTM   TO WRK-OUT-VLROPER-FRTM-X.
           MOVE WRK-INP-TPTAXA-FRTM    TO WRK-OUT-TPTAXA-FRTM
           MOVE WRK-INP-VLRTARO-FRTM   TO WRK-OUT-VLRTARO-FRTM-X.
           MOVE WRK-INP-TXJUROS-FRTM   TO WRK-OUT-TXJUROS-FRTM-X.
           MOVE WRK-INP-TXDESC-FRTM    TO WRK-OUT-TXDESC-FRTM
           MOVE WRK-INP-VLRTART-FRTM   TO WRK-OUT-VLRTART-FRTM-X.
           MOVE WRK-INP-DOPERAC-FRTM   TO WRK-OUT-DOPERAC-FRTM
           MOVE WRK-INP-DVCTFIM-FRTM   TO WRK-OUT-DVCTFIM-FRTM
           MOVE WRK-INP-VLRJUROS-FRTM  TO WRK-OUT-VLRJUROS-FRTM-X.
           MOVE WRK-INP-PRZMEDIO-FRTM  TO WRK-OUT-PRZMEDIO-FRTM-X.
           MOVE WRK-INP-VLRIOF-FRTM    TO WRK-OUT-VLRIOF-FRTM-X.
           MOVE WRK-INP-QTDTIT-FRTM    TO WRK-OUT-QTDTIT-FRTM-X.
           MOVE WRK-INP-VLRLIQ-FRTM    TO WRK-OUT-VLRLIQ-FRTM-X.
           MOVE WRK-INP-GARANTIA-FRTM  TO WRK-OUT-GARANTIA-FRTM
           MOVE WRK-INP-CONTRC-FRTM    TO WRK-OUT-CONTRC-FRTM.
           MOVE WRK-INP-CODMEIO-FRTM   TO WRK-OUT-CODMEIO-FRTM.
           MOVE WRK-INP-MEIOENT-FRTM   TO WRK-OUT-MEIOENT-FRTM.
BRQ001     MOVE WRK-INP-LIBMID-FRTM    TO WRK-OUT-LIBMID-FRTM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMEST-FRTM        TO WRK-ENT-TIMESTAMP
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
           MOVE WRK-INP-TIMEST-FRTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FRTM
           MOVE 'I '                       TO WRK-ENT-OPCAO
           MOVE SPACES                     TO WRK-CHAMADOR(IND-1 - 1)
           MOVE ZEROS                      TO WRK-PAGINA(IND-1 - 1)
           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0618'           TO COMU-TRANSACAO-FRTM.
           MOVE LENGTH OF COMU-AREA-IMS
                                     TO COMU-LL-FRTM
           MOVE COMU-AREA-IMS        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMEST-FRTM        TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0618'          TO WRK-TELA
                                       COMU-TRANSACAO-FRTM.
           MOVE '5'                 TO COMU-PFK-FRTM.
           MOVE COMU-AREA-IMS       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TIMEST-FRTM        TO WRK-ENT-TIMESTAMP.
           MOVE 1                          TO WRK-ENT-PAGINA.
           MOVE 'ET'                       TO WRK-ENT-OPCAO.

           PERFORM 8200-ACESSAR-DCOM7995.

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0618'         TO WRK-TELA
                                      COMU-TRANSACAO-FRTM.
           MOVE 'A'                TO COMU-PFK-FRTM.
           MOVE COMU-AREA-IMS      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1241-TRATAR-CONTINUACAO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-COMUN-TOTAL
                      COMU-AREA-IMS.

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE WRK-INP-TIMEST-FRTM        TO WRK-ENT-TIMESTAMP
                                              COMU-TIMESTAMP-FRTM

           MOVE 1                          TO WRK-ENT-PAGINA
           MOVE 'CP'                       TO WRK-ENT-OPCAO
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE WRK-SAI-DADOS              TO WRK-AREA-COMUN-TOTAL

           MOVE WRK-INP-NOMECLI-FRTM    TO WRK-NOMECLI
           MOVE WRK-INP-CNPJCPF-FRTM    TO WRK-CNPJCPF
           MOVE WRK-INP-AGENCIA-FRTM    TO WRK-AGENCIA
           MOVE WRK-INP-CONTA-FRTM      TO WRK-CONTA
           MOVE WRK-INP-SITU1-FRTM      TO WRK-SITU1
           MOVE WRK-INP-SITU2-FRTM      TO WRK-SITU2
           MOVE WRK-INP-SITU3-FRTM      TO WRK-SITU3

           IF  WRK-INP-DTADIT-FRTM      EQUAL SPACES OR LOW-VALUES
               MOVE '01.01.0001'        TO WRK-DATA-ADITAMENTO
           ELSE
               MOVE WRK-INP-DTADIT-FRTM TO WRK-DATA-ADITAMENTO
           END-IF

           MOVE WRK-INP-SITU4-FRTM      TO WRK-SITU4
           MOVE WRK-INP-SITU5-FRTM      TO WRK-SITU5
           MOVE WRK-INP-NUM1CONS-FRTM   TO WRK-NUM1CONS
           MOVE WRK-INP-NUM2CONS-FRTM   TO WRK-NUM2CONS
           MOVE WRK-INP-NUM3CONS-FRTM   TO WRK-NUM3CONS
           MOVE WRK-INP-CODPROD-FRTM    TO WRK-PRODUTO
           MOVE WRK-INP-DESPROD-FRTM    TO WRK-DESC-PRODUTO
           MOVE WRK-INP-CODSUBP-FRTM    TO WRK-SUBPRODUTO
           MOVE WRK-INP-DESSUBP-FRTM    TO WRK-DESC-SPROD
           MOVE WRK-INP-VLRTARO-FRTM    TO WRK-VLRTARO
           MOVE WRK-INP-VLRTART-FRTM    TO WRK-VLRTART
           MOVE WRK-INP-DVCTFIM-FRTM    TO WRK-DVCTFIM
           MOVE WRK-INP-DOPERAC-FRTM    TO WRK-DOPERAC
           MOVE WRK-INP-VLRJUROS-FRTM   TO WRK-VLRJUROS
           MOVE WRK-INP-PRZMEDIO-FRTM   TO WRK-PRZMEDIO
           MOVE WRK-INP-VLRIOF-FRTM     TO WRK-VLRIOF
           MOVE WRK-INP-QTDTIT-FRTM     TO WRK-QTDTIT
           MOVE WRK-INP-VLRLIQ-FRTM     TO WRK-VLRLIQ
           MOVE WRK-INP-CODMEIO-FRTM    TO WRK-CODMEIO
           MOVE WRK-INP-MEIOENT-FRTM    TO WRK-MEIOENT
           MOVE WRK-INP-TPDES-FRTM      TO WRK-TPDES
           MOVE WRK-INP-GARANTIA-FRTM   TO WRK-GARANTIA
           MOVE WRK-INP-CARTCONS-FRTM   TO WRK-CARTCONS
           MOVE WRK-INP-CONTRC-FRTM-N   TO WRK-CONTRC
           MOVE WRK-INP-TPTAXA-FRTM     TO WRK-TPTAXA

           MOVE 'I '                       TO WRK-ENT-OPCAO

           PERFORM VARYING IND-1               FROM 1 BY 1
                     UNTIL IND-1               GREATER 15


               IF  WRK-CHAMADOR(IND-1)         EQUAL SPACES
                   MOVE 'DCOM0618'             TO WRK-CHAMADOR(IND-1)
                                                  COMU-TRANSACAO-FRTM
                   MOVE 99                     TO IND-1
               END-IF
           END-PERFORM.

           MOVE WRK-AREA-COMUN-TOTAL       TO WRK-ENT-DADOS
           PERFORM 8200-ACESSAR-DCOM7995
           MOVE LENGTH OF COMU-AREA-IMS    TO COMU-LL-FRTM

           MOVE COMU-AREA-IMS              TO WRK-MENSAGEM
           MOVE WRK-CHNG                   TO WRK-FUNCAO.
           MOVE 'DCOM0619'                 TO WRK-TELA.

      *----------------------------------------------------------------*
       1241-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*
           MOVE LENGTH OF WRK-OUTPUT-FRTM
                                       TO  WRK-660-LL-MENSAGEM-FRTM
                                           WRK-OUT-LL-FRTM

           MOVE LENGTH OF WRK-660-DCOMFRTM
                                       TO  WRK-660-LL-AREA-FRTM

           CALL 'BRAD0660'             USING WRK-OUTPUT-FRTM
                                             WRK-660-DCOMFRTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-FRTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
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
PROC01                                 TO COMU-LL-FRTM.
PROC01     MOVE WRK-INP-TIMEST-FRTM    TO WRK-ENT-TIMESTAMP
PROC01                                    COMU-TIMESTAMP-FRTM.
PROC01
PROC01     MOVE 1                      TO WRK-ENT-PAGINA
PROC01     MOVE 'CP'                   TO WRK-ENT-OPCAO
PROC01     PERFORM 8200-ACESSAR-DCOM7995
PROC01     MOVE WRK-SAI-DADOS          TO WRK-AREA-COMUN-TOTAL
PROC01
PROC01     MOVE WRK-TIMESTAMP          TO WRK-COMU-TIME-AUX-FRTM.
PROC01     MOVE WRK-INP-DANO-OPER-FRTM TO WRK-COMU-DANO-FRTM.
PROC01     MOVE WRK-INP-NSEQ-OPER-FRTM TO WRK-COMU-NSEQ-FRTM.
PROC01     MOVE 'DCOM3924'             TO WRK-TELA.
PROC01     MOVE 'DCOM0618'             TO COMU-TRANSACAO-FRTM.
PROC01     MOVE WRK-CHNG               TO WRK-FUNCAO.
PROC01     MOVE COMU-AREA-IMS          TO WRK-MENSAGEM.
PROC01
PROC01*----------------------------------------------------------------*
PROC01 1400-99-FIM.                    EXIT.
PROC01*----------------------------------------------------------------*

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
               MOVE 'DCOM0618'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0618'         TO ERR-PGM
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
