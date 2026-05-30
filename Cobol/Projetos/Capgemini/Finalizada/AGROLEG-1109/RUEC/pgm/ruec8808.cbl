      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8808.
       AUTHOR.     MARINA FUJITA.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8808                                    *
      *    ANALISTA....:   MARINA FUJITA           - SONDA PROCWORK    *
      *    DATA........:   21/06/2012                                  *
      *    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
      *                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS P/ ENVIO RECOR NAS TABELAS DB2  *
      *                    RUECB099, RUECB09D, RUECB09E, RUECB09B,     *
      *                    RUECB09F                                    *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC58 - AREA DE RETORNO DO MODULO.                       *
      *    I#RUEC62 - AREA DE RETORNO DO MODULO.                       *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *    MESU9032 - OBTER DADOS DA AGENCIA                           *
      *    RUEC8800 - OBTER EMPREENDIMENTO E SISTEMA DE PRODUCAO       *
HE0814*================================================================*
HE0814*                    S O N D A  P R O C W O R K                  *
HE0814*----------------------------------------------------------------*
HE0814*    ANALISTA....:   HEYDI MARIGO            - BRADESCO          *
HE0814*    DATA........:   15/08/2014                                  *
HE0814*    PROJETO.....:   ATUALIZACAO CATALOGO 4.03                   *
HE0814*                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
HE0814*----------------------------------------------------------------*
HE0814*    OBJETIVO....:   AJUSTAR ENVIO DO CAMPO TP-GAR-EMPREEND.     *
HE0814*================================================================*
EG0616*================================================================*
EG0616*                      A L T E R A C A O                         *
EG0616*================================================================*
EG0616*    ANALISTA....:   EVANDRO GUIMARAES       - BSI TECNOLOGIA    *
EG0616*    DATA........:   JUNHO/2016                                  *
EG0616*    OBJETIVO....:   ENVIAR MODALIDADE DE SEGURO IGUAL A         *
EG0616*                    '3-OUTRO SEGURO' QUANDO O CODIGO DE ORIGEM  *
EG0616*                    FOR IGUAL A 28 OU 34.                       *
BR1119*================================================================*
BR1119*    ANALISTA....:   LEILA FREIRE            - BRQ               *
BR1119*    DATA........:   NOVEMBR0/2019                               *
BR1119*    OBJETIVO....:   CARREGAR FUMAGEIRA                          *
BR1119*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** INICIO DA WORKING RUEC8808 ***'.
      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*
       77  WRK-MODULO                  PIC X(008)     VALUE SPACES.
       77  WRK-VRECTA-BRUTA            PIC S9(15)V99 COMP-3 VALUE ZEROS.
       77  WRK-NPCELA-CONTR            PIC S9(03) COMP-3 VALUE ZEROS.
       77  WRK-NPSSOA-CONTR            PIC S9(03) COMP-3 VALUE ZEROS.
       77  WRK-DINIC-SAFRA             PIC S9(04) COMP VALUE ZEROS.
       77  WRK-DFIM-SAFRA              PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CMUN-RURAL              PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CFNALD-RURAL            PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CATVDD-RURAL            PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CORIGE-REC              PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CPRODT-RURAL            PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CUND-MEDD-FINCD         PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CUND-MEDD-PROVV         PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CESPCE-TITLO-GARNT      PIC S9(04) COMP VALUE ZEROS.
       77  WRK-VRECTA-BRUTA-EMPTO      PIC S9(04) COMP VALUE ZEROS.
       77  WRK-CTPO-PROTR-RURAL        PIC S9(04) COMP VALUE ZEROS.

STEFAB 01  FILLER.
STEFAB     03  WRK-N05                 PIC +9(05)      VALUE ZEROS.
STEFAB     03  FILLER  REDEFINES  WRK-N05.
STEFAB         05  FILLER              PIC  X(01).
STEFAB         05  WRK-N05S            PIC  9(05).

EG0617 77  WRK-B040-CCONTR-N           PIC S9(04) COMP VALUE ZEROS.
EG0617 77  WRK-B040-CSEQ-MATR-RURAL-N  PIC S9(04) COMP VALUE ZEROS.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 01  FILLER                      PIC X(032)          VALUE
EG0617     '*** CONTROLES DE CURSOR      ***'.
EG0617*---------------------------------------------------------------*
EG0617
EG0617 01  FILLER.
EG0617     05  WRK-FIM-B040            PIC X(01)          VALUE SPACES.
EG0617     05  WRK-FIM-B059            PIC X(01)          VALUE SPACES.
EG0617
      *---------------------------------------------------------------*

       01  WRK-INI1                PIC S9(04)  COMP-3 VALUE ZEROS.
       01  WRK-FIM1                PIC S9(04)  COMP-3 VALUE ZEROS.
       01  WRK-INI2                PIC S9(04)  COMP-3 VALUE ZEROS.
       01  WRK-FIM2                PIC S9(04)  COMP-3 VALUE ZEROS.
       01  WRK-INI3                PIC S9(04)  COMP-3 VALUE ZEROS.
       01  WRK-FIM3                PIC S9(04)  COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*

       01  WRK-CTRL-CHAR           PIC  X(02)   VALUE SPACES.
       01  FILLER   REDEFINES   WRK-CTRL-CHAR.
           05  WRK-CTRL-NUM        PIC  9(02).

STE001 01  WRK-CTRL-NUM-2          PIC +9(02)   VALUE ZEROS.
STE001 01  FILLER   REDEFINES   WRK-CTRL-NUM-2.
STE001     05  FILLER              PIC  X(01).
STE001     05  WRK-CTRL-CHAR2      PIC  X(02).

DK.... 01  WRK-CMUN-RURAL9         PIC -9(09)          VALUE ZEROS.
       01  FILLER   REDEFINES  WRK-CMUN-RURAL9.
DK....     05  FILLER              PIC  X(01).
           05  FILLER              PIC  9(03).
           05  WRK-CMUN-RURAL6     PIC  9(06).

DK.... 01  WRK-QUND-FINCD11      PIC -9(09)V9(2)       VALUE ZEROS.
       01  FILLER   REDEFINES  WRK-QUND-FINCD11.
DK....     05  FILLER            PIC  X(01).
           05  FILLER            PIC  9(02).
           05  WRK-QUND-FINCD9   PIC  9(07)V9(2).

       01  FILLER.
           03  WRK-CEDULA-RECOR        PIC  X(17)      VALUE SPACES.
           03  FILLER   REDEFINES  WRK-CEDULA-RECOR.
               05  WRK-CJUNC-DEPDC     PIC  9(05).
               05  WRK-CCDULA          PIC  9(12).

DK....     03  WRK-CTPO-GARNT-RECOR    PIC -9(03)      VALUE ZEROS.
           03  FILLER   REDEFINES  WRK-CTPO-GARNT-RECOR.
DK....         05  FILLER              PIC  X(02).
               05  WRK-CTPO-GARNT-X    PIC  X(02).

           03  WRK-TAXA-JUROS          PIC -9(13)V9(05) VALUE ZEROS.
           03  FILLER   REDEFINES  WRK-TAXA-JUROS.
DK....         05  FILLER              PIC  X(11).
               05  WRK-TAXA-JUROS-3V5  PIC  9(03)V9(05).

       01  WRK-DVCTO-CONTR             PIC X(10) VALUE SPACES.
       01  WRK-DATA-DB2                PIC X(10) VALUE SPACES.
       01  FILLER      REDEFINES  WRK-DATA-DB2.
           05  FILLER                  PIC X(06).
           05  WRK-ANO-DB2             PIC 9(04).

       01  WRK-SAFRA-DB2               PIC 9(06) VALUE ZEROS.
       01  FILLER.
           05  WRK-INI-SAFRA           PIC 9(04)  COMP-3 VALUE ZEROS.
           05  WRK-FIM-SAFRA           PIC 9(04)  COMP-3 VALUE ZEROS.
           05  WRK-ANO-DEMIS           PIC 9(04)  COMP-3 VALUE ZEROS.

STE001 01  WRK-MANEJO                  PIC +9(05)         VALUE ZEROS.
.      01  FILLER                      REDEFINES  WRK-MANEJO.
.          05  FILLER                  PIC  X(01).
STE001     05  WRK-MANEJO-N            PIC  9(05).

STE001 01  WRK-CRUR2G4C                PIC X(08)       VALUE 'CRUR2G4C'.
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RUEC8800 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC58'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO MESU9116 ***'.
      *---------------------------------------------------------------*

       COPY I#MESUM9.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO MESU9032 ***'.
      *---------------------------------------------------------------*

       01  WRK-MESU9032.
           03  WRK-ENTRADA-MESU9032.
               05  WRK-NOME-PGM        PIC X(08)     VALUE SPACES.
               05  WRK-COD-RETORNO     PIC 9(02)     VALUE ZEROS.
               05  WRK-AGENCIA         PIC 9(05)     VALUE ZEROS.
           03  WRK-SAIDAS-MESU9032.
               05  WRK-NOME            PIC X(20)     VALUE SPACES.
               05  WRK-ENDERECO        PIC X(30)     VALUE SPACES.
               05  WRK-CIDADE          PIC X(25)     VALUE SPACES.
               05  WRK-ESTADO          PIC X(02)     VALUE SPACES.
               05  WRK-CEP             PIC 9(05)     VALUE ZEROS.
               05  WRK-REGIO-DIRET     PIC 9(03)     VALUE ZEROS.
               05  WRK-JUNC-REGIO      PIC 9(05)     VALUE ZEROS.
               05  WRK-NOVOCEP         PIC 9(09)     VALUE ZEROS.
               05  WRK-NOME-GER        PIC X(30)     VALUE SPACES.
               05  WRK-DT-INAUGUR      PIC X(10)     VALUE SPACES.
               05  WRK-DT-ENCRESU      PIC X(10)     VALUE SPACES.
               05  WRK-LOJA            PIC 9(05)     VALUE ZEROS.
               05  WRK-CONTROLE        PIC X(02)     VALUE SPACES.

       01  WRK-POOL7100-MESU9032       PIC X(107)    VALUE SPACES.
       01  WRK-SQLCA-MESU9032          PIC X(136)    VALUE SPACES.
       01  WRK-AMBIENTE-MESU9032       PIC X(01)     VALUE SPACES.
       01  WRK-IO-PCB-MESU9032         PIC X(32)     VALUE SPACES.
       01  WRK-ALT-PCB-MESU9032        PIC X(32)     VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA BRAD7600   ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC X(20)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA FORMATACAO DE ERRO ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

STE001*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(50)         VALUE
.          '*** AREAS DA CRUR2G4C ***'.
.     *----------------------------------------------------------------*
.
.      01  WRK-AREA-CRUR2G4C.
.          COPY CRURW00W.
.          COPY CRURWG4I.
STE001
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

201502     EXEC SQL INCLUDE RURCB006 END-EXEC.
201502
           EXEC SQL INCLUDE RUFIB002 END-EXEC.

           EXEC SQL INCLUDE RUECB007 END-EXEC.

           EXEC SQL INCLUDE RUECV012 END-EXEC.

           EXEC SQL INCLUDE RUECB014 END-EXEC.

           EXEC SQL INCLUDE RUECB061 END-EXEC.

           EXEC SQL INCLUDE RUECB063 END-EXEC.

           EXEC SQL INCLUDE RUECB099 END-EXEC.

           EXEC SQL INCLUDE RUECB09B END-EXEC.

           EXEC SQL INCLUDE RUECB09D END-EXEC.

           EXEC SQL INCLUDE RUECB09E END-EXEC.

           EXEC SQL INCLUDE RUECB09F END-EXEC.

           EXEC SQL INCLUDE RUECB09G END-EXEC.

           EXEC SQL INCLUDE RURCB000 END-EXEC.

           EXEC SQL INCLUDE RURCB014 END-EXEC.

           EXEC SQL INCLUDE RURCB018 END-EXEC.

           EXEC SQL INCLUDE RURCB022 END-EXEC.

           EXEC SQL INCLUDE RURCB028 END-EXEC.

           EXEC SQL INCLUDE RUECB09J END-EXEC.

EG0617*---------------------------------------------------------------*
EG0617 01  FILLER                      PIC X(032)          VALUE
EG0617     '*** INCLUDE RUECB040         ***'.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL INCLUDE RUECB040 END-EXEC.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 01  FILLER                      PIC X(032)          VALUE
EG0617     '*** INCLUDE RUECB059         ***'.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL INCLUDE RUECB059 END-EXEC.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 01  FILLER                      PIC X(032)          VALUE
EG0617     '*** INCLUDE RUECB056         ***'.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL INCLUDE RUECB056 END-EXEC.
EG0617
           EXEC SQL
             DECLARE CSR01-RUECV012 CURSOR FOR
              SELECT CTPO_EVNTO_RURAL
                   , DEVNTO
                   , VEVNTO_RURAL
                   , VPCELA_REC_PPRIO
                FROM  DB2PRD.V01EVNTO_RURAL
               WHERE  CCONTR             = :RUECV012.CCONTR
                 AND  (CTPO_EVNTO_RURAL BETWEEN :WRK-INI1 AND
                                                :WRK-FIM1)
               ORDER BY DEVNTO ASC
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-RUECB014 CURSOR FOR
              SELECT CTPO_PSSOA
                   , CTPO_RESP_PSSOA
                   , CCGC_CPF
                   , CFLIAL_CGC
                   , CCTRL_CPF_CGC
                FROM  DB2PRD.PESSOA_CONTR_RURAL
               WHERE  CCONTR             = :RUECB014.CCONTR
                 AND  CTPO_RESP_PSSOA    = :RUECB014.CTPO-RESP-PSSOA
               ORDER BY CSEQ_PSSOA ASC
           END-EXEC.

           EXEC SQL
             DECLARE CSR01-RUECB09G CURSOR FOR
              SELECT CPRODT_RURAL
                   , NPRODT_RURAL_CSCDO
                FROM  DB2PRD.TPRODT_RURAL_CSCDO
               WHERE  CCONTR_CREDT_RURAL = :RUECB09G.CCONTR-CREDT-RURAL
               ORDER BY CPRODT_RURAL ASC
           END-EXEC.

EG0617     EXEC SQL
EG0617       DECLARE CSR01-RUECB040 CURSOR FOR
EG0617       SELECT CSEQ_IMOV_RURAL
EG0617       ,      VALUE ( CCONTR          , 0 )
EG0617       ,      VALUE ( CSEQ_MATR_RURAL , 0 )
EG0617       FROM   DB2PRD.MATR_CONTR_RURAL
EG0617       WHERE  CCONTR             = :RUECB040.CCONTR
EG0617       ORDER  BY CSEQ_IMOV_RURAL
EG0617     END-EXEC.
EG0617
EG0617     EXEC SQL
EG0617       DECLARE CSR01-RUECB059 CURSOR FOR
EG0617       SELECT CCGC_CPF
EG0617       ,      CFLIAL_CGC
EG0617       ,      CCTRL_CGC_CPF
EG0617       FROM   DB2PRD.TPPRIE_IMOV_ARREN
EG0617       WHERE  CCLUB              = :RUECB059.CCLUB
EG0617       AND    CSEQ_IMOV_RURAL    = :RUECB059.CSEQ-IMOV-RURAL
EG0617       AND    CSEQ_ATULZ_IMOV    = :RUECB059.CSEQ-ATULZ-IMOV
EG0617     END-EXEC.
EG0617
      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8808 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC62'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC62-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL  'CKRS1000'.

           MOVE ZEROS              TO RUEC62-COD-RET.

           PERFORM  1000-PROCESSAMENTO-PRINCIPAL.

           PERFORM  4000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAMENTO-PRINCIPAL SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL7600'  USING  WRK-DATA-HORA.

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

           IF  RUEC62-COD-RET  EQUAL  ZEROS
               PERFORM 3000-OBTEM-DADOS
201502         PERFORM 3010-OBTEM-DADOS-RURCB006
               PERFORM 3100-GERA-CONTRATO
               PERFORM 3200-GERA-PARCELAS
               PERFORM 3300-GERA-PESSOAS
               PERFORM 3400-GERA-PRODUTOS-CONSORC
               PERFORM 3900-ATUALIZA-CEDULA
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC62-PGM-CHAMADOR EQUAL SPACES
               MOVE 01             TO RUEC62-COD-RET
               MOVE 'PGM CHAMADOR INVALIDO'
                                   TO RUEC62-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC62-CCONTR       NOT   NUMERIC)
           OR (RUEC62-CCONTR       EQUAL ZEROS)
               MOVE 01             TO RUEC62-COD-RET
               MOVE 'CONTRATO INVALIDO'
                                   TO RUEC62-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF  RUEC62-PROCESS  NOT  EQUAL  'B'  AND 'O'
               MOVE 01             TO RUEC62-COD-RET
               MOVE 'FORMA PROCESSAMENTO INVALIDO'
                                   TO RUEC62-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OBTEM-DADOS            SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR     TO CCONTR             OF RUFIB002

           EXEC SQL
               SELECT   CCONTR
                      , CCDULA
                      , DCDULA
                      , DINIC_SAFRA
                      , DVCTO_CONTR
                      , VFINAN
                      , QUND_FINCD_RURAL
                      , QUND_PROD_PROVV
                      , VREC_PPRIO
                      , DFIM_SAFRA
                      , CJUNC_DEPDC
                      , CMUN_RURAL
                      , CFNALD_RURAL
                      , CATVDD_RURAL
                      , CORIGE_REC
                      , CPRODT_RURAL
                      , CTPO_PROTR_RURAL
                      , CUND_MEDD_FINCD
                      , CUND_MEDD_PROVV
                      , CESPCE_TITLO_GARNT
                      , DIMPRE_CONTR_RURAL
                      , CCTRO_CUSTO
                      , CCESTA_SAFRA_RURAL
                      , CMODLD_RURAL
                      , CVAR_PRODT_RURAL
                      , CINDCD_ZNMTO
                      , CINDCD_CSCIO
                      , CTPO_AGROP_RURAL
                      , CTPO_INTGC_RURAL
                      , CINDCD_UND_PRDTV
                      , CTPO_IGCAO_RURAL
                      , CTPO_CTIVO_RURAL
                      , CCICLO_PROD_RURAL
                      , CREFT_RECOR
                      , VRECTA_BRUTA_EMPTO
PRI499                , CCLIMT_RURAL_BACEN
               INTO     :RUFIB002.CCONTR
                      , :RUFIB002.CCDULA
                      , :RUFIB002.DCDULA
                      , :RUFIB002.DINIC-SAFRA      :WRK-DINIC-SAFRA
                      , :RUFIB002.DVCTO-CONTR
                      , :RUFIB002.VFINAN
                      , :RUFIB002.QUND-FINCD-RURAL
                      , :RUFIB002.QUND-PROD-PROVV
                      , :RUFIB002.VREC-PPRIO
                      , :RUFIB002.DFIM-SAFRA       :WRK-DFIM-SAFRA
                      , :RUFIB002.CJUNC-DEPDC
                      , :RUFIB002.CMUN-RURAL       :WRK-CMUN-RURAL
                      , :RUFIB002.CFNALD-RURAL     :WRK-CFNALD-RURAL
                      , :RUFIB002.CATVDD-RURAL     :WRK-CATVDD-RURAL
                      , :RUFIB002.CORIGE-REC       :WRK-CORIGE-REC
                      , :RUFIB002.CPRODT-RURAL     :WRK-CPRODT-RURAL
                      , :RUFIB002.CTPO-PROTR-RURAL :WRK-CTPO-PROTR-RURAL
                      , :RUFIB002.CUND-MEDD-FINCD  :WRK-CUND-MEDD-FINCD
                      , :RUFIB002.CUND-MEDD-PROVV  :WRK-CUND-MEDD-PROVV
                      , :RUFIB002.CESPCE-TITLO-GARNT
                                                 :WRK-CESPCE-TITLO-GARNT
                      , :RUFIB002.DIMPRE-CONTR-RURAL
                      , :RUFIB002.CCTRO-CUSTO
                      , :RUFIB002.CCESTA-SAFRA-RURAL
                      , :RUFIB002.CMODLD-RURAL
                      , :RUFIB002.CVAR-PRODT-RURAL
                      , :RUFIB002.CINDCD-ZNMTO
                      , :RUFIB002.CINDCD-CSCIO
                      , :RUFIB002.CTPO-AGROP-RURAL
                      , :RUFIB002.CTPO-INTGC-RURAL
                      , :RUFIB002.CINDCD-UND-PRDTV
                      , :RUFIB002.CTPO-IGCAO-RURAL
                      , :RUFIB002.CTPO-CTIVO-RURAL
                      , :RUFIB002.CCICLO-PROD-RURAL
                      , :RUFIB002.CREFT-RECOR
                      , :RUFIB002.VRECTA-BRUTA-EMPTO
                                                 :WRK-VRECTA-BRUTA-EMPTO
PRI499                , :RUFIB002.CCLIMT-RURAL-BACEN
                 FROM DB2PRD.CONTRATO_RURAL
                WHERE CCONTR              = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL   ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC62-COD-RET
               MOVE 'CONTRATO INEXISTENTE NO RUFIB002'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  CREFT-RECOR OF RUFIB002 NOT  EQUAL  SPACES  AND ZEROS
               MOVE 04                  TO RUEC62-COD-RET
               MOVE 'NUMERO RECOR JA EXISTENTE P/ CONTRATO'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  WRK-DINIC-SAFRA          LESS  ZEROS
               MOVE ZEROS               TO DINIC-SAFRA       OF RUFIB002
           END-IF.

           IF  WRK-DFIM-SAFRA           LESS  ZEROS
               MOVE ZEROS               TO DFIM-SAFRA        OF RUFIB002
           END-IF.

           IF  WRK-CMUN-RURAL           LESS  ZEROS
               MOVE ZEROS               TO CMUN-RURAL    OF RUFIB002
           END-IF.

           IF  WRK-CFNALD-RURAL         LESS  ZEROS
               MOVE ZEROS               TO CFNALD-RURAL  OF RUFIB002
           END-IF.

           IF  WRK-CATVDD-RURAL         LESS  ZEROS
               MOVE ZEROS               TO CATVDD-RURAL  OF RUFIB002
           END-IF.

           IF  WRK-CORIGE-REC           LESS  ZEROS
               MOVE ZEROS               TO CORIGE-REC    OF RUFIB002
           END-IF.

           IF  WRK-CPRODT-RURAL         LESS  ZEROS
               MOVE ZEROS               TO CPRODT-RURAL  OF RUFIB002
           END-IF.

           IF  WRK-CTPO-PROTR-RURAL     LESS  ZEROS
               MOVE ZEROS               TO CTPO-PROTR-RURAL OF RUFIB002
           END-IF.

           IF  WRK-CUND-MEDD-FINCD      LESS  ZEROS
               MOVE ZEROS               TO CUND-MEDD-FINCD OF RUFIB002
           END-IF.

           IF  WRK-CUND-MEDD-PROVV      LESS  ZEROS
               MOVE ZEROS               TO CUND-MEDD-PROVV OF RUFIB002
           END-IF.

           IF  WRK-CESPCE-TITLO-GARNT   LESS  ZEROS
               MOVE ZEROS            TO CESPCE-TITLO-GARNT OF RUFIB002
           END-IF.

           IF  WRK-VRECTA-BRUTA-EMPTO  LESS  ZEROS
               MOVE ZEROS            TO VRECTA-BRUTA-EMPTO OF RUFIB002
           END-IF.

STEFAB     MOVE RUEC62-CCONTR        TO CCONTR             OF RUECB007.
.
.          EXEC SQL
.              SELECT  VALUE(COUTRO_TRATO_SOLO, 0)
.                INTO :RUECB007.COUTRO-TRATO-SOLO
.              FROM    DB2PRD.CEDULA_TEMPR
.              WHERE   CCONTR           = :RUECB007.CCONTR
.          END-EXEC.
.
.          IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                 EQUAL 'W')
.              MOVE 99                  TO RUEC62-COD-RET
.              MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
.              MOVE 'DB2'               TO ERR-TIPO-ACESSO
.              MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
.              MOVE 'SELECT'            TO ERR-FUN-COMANDO
.              MOVE SQLCODE             TO ERR-SQL-CODE
.              MOVE '0110'              TO ERR-LOCAL
.              MOVE SPACES              TO ERR-SEGM
.              PERFORM 4000-ROTINA-FIM
.          END-IF.
.
.          IF  SQLCODE                  EQUAL +100
.              MOVE ZEROS               TO WRK-N05
.          ELSE
.              MOVE COUTRO-TRATO-SOLO   OF RUECB007
.                                       TO WRK-N05
STEFAB     END-IF.

           INITIALIZE  RUEC58-AREA.
           MOVE RUEC62-PGM-CHAMADOR            TO RUEC58-PGM-CHAMADOR
           MOVE CCONTR OF RUFIB002             TO RUEC58-CONTRATO
           MOVE CFNALD-RURAL OF RUFIB002       TO RUEC58-FINALIDADE
           MOVE CATVDD-RURAL OF RUFIB002       TO RUEC58-ATIVIDADE
           MOVE CMODLD-RURAL OF RUFIB002       TO RUEC58-MODALIDADE
           MOVE CPRODT-RURAL OF RUFIB002       TO RUEC58-PRODUTO
           MOVE CVAR-PRODT-RURAL OF RUFIB002   TO RUEC58-VARIEDADE
           MOVE CCESTA-SAFRA-RURAL OF RUFIB002 TO RUEC58-CESTA-SAFRA
           MOVE CINDCD-ZNMTO OF RUFIB002       TO RUEC58-ZONEAMENTO
           MOVE CINDCD-CSCIO OF RUFIB002       TO RUEC58-CONSORCIO
           MOVE CTPO-AGROP-RURAL OF RUFIB002   TO RUEC58-TIPO-AGROP
           MOVE CTPO-INTGC-RURAL OF RUFIB002   TO RUEC58-TIPO-INTEG
           MOVE CINDCD-UND-PRDTV OF RUFIB002   TO RUEC58-GRAO-SEMENTE
           MOVE CTPO-IGCAO-RURAL OF RUFIB002   TO RUEC58-TIPO-IRRIG
           MOVE CTPO-CTIVO-RURAL OF RUFIB002   TO RUEC58-TIPO-CULTIVO
           MOVE CCICLO-PROD-RURAL  OF RUFIB002 TO RUEC58-CICLO-PROD
PRI499     MOVE CCLIMT-RURAL-BACEN OF RUFIB002 TO RUEC58-CLIMA-PROD
           MOVE DCDULA OF RUFIB002             TO RUEC58-DCDULA
STEFAB     MOVE WRK-N05S           TO RUEC58-COUTRO-TRATO-SOLO


           MOVE 'RUEC8800'             TO  WRK-MODULO
           CALL  WRK-MODULO        USING   RUEC58-AREA

           IF RUEC58-COD-RET            EQUAL   99
               MOVE RUEC58-SQLCA        TO SQLCA
               MOVE RUEC58-ERRO-AREA    TO ERRO-AREA
               MOVE RUEC58-COD-RET      TO RUEC62-COD-RET
               MOVE RUEC58-MENSAGEM     TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF RUEC58-COD-RET     NOT EQUAL ZEROS AND 04 AND 05
               MOVE RUEC58-COD-RET      TO RUEC62-COD-RET
               MOVE RUEC58-MENSAGEM     TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CORIGE-REC OF RUFIB002  TO CORIGE-REC OF RURCB000.
           MOVE CMUN-RURAL OF RUFIB002  TO CMUN-RURAL OF RURCB014.
           MOVE CESPCE-TITLO-GARNT  OF RUFIB002
                                    TO CESPCE-TITLO-GARNT  OF RURCB018
           MOVE CTPO-PROTR-RURAL    OF RUFIB002
                                    TO CTPO-PROTR-RURAL    OF RURCB022
           PERFORM 3010-SELECT-RURCB000.
           MOVE CPLANO-REFIN-RURAL      TO CPLANO-REFIN.
           PERFORM 3020-SELECT-RURCB014.
           PERFORM 3030-SELECT-RURCB018.
           PERFORM 3035-SELECT-RURCB022.
           PERFORM 3040-SELECT-RURCB028.
           PERFORM 3050-SELECT-RUECV012-JUROS.
           PERFORM 3060-CHAMADA-MESU.
           PERFORM 3070-SELECT-RUECV012-DVCTO.

      *---------------------------------------------------------------*
       3000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

201502*---------------------------------------------------------------*
201502 3010-OBTEM-DADOS-RURCB006   SECTION.
201502*---------------------------------------------------------------*
201502
201502     MOVE  CPRODT-RURAL          OF RUFIB002
201502                                 TO CPRODT-RURAL OF RURCB006.
201502
201502     EXEC SQL
201502       SELECT
201502         CCTCIA_PROD_PROVV
201502       INTO
201502         :RURCB006.CCTCIA-PROD-PROVV
201502       FROM  DB2PRD.PRODUTO_RURAL
201502       WHERE CPRODT_RURAL        = :RURCB006.CPRODT-RURAL
201502     END-EXEC.
201502
201502     IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )
201502     OR (SQLWARN0                EQUAL 'W'                )
201502         MOVE 99                 TO RUEC62-COD-RET
201502         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
201502         MOVE 'DB2'              TO ERR-TIPO-ACESSO
201502         MOVE 'RURCB006'         TO ERR-DBD-TAB
201502         MOVE 'SELECT'           TO ERR-FUN-COMANDO
201502         MOVE SQLCODE            TO ERR-SQL-CODE
201502         MOVE '3010'             TO ERR-LOCAL
201502         MOVE SPACES             TO ERR-SEGM
201502         PERFORM 4000-ROTINA-FIM
201502     END-IF.
201502
201502     IF  SQLCODE                 EQUAL +100
201502         MOVE ZEROS              TO CCTCIA-PROD-PROVV OF RURCB006
201502     END-IF.
201502
201502*---------------------------------------------------------------*
201502 3010-99-FIM.                EXIT.
201502*---------------------------------------------------------------*
201502
      *---------------------------------------------------------------*
       3010-SELECT-RURCB000        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CPLANO_REFIN_RURAL
                  , CPROG_RURAL
                  INTO    :RURCB000.CPLANO-REFIN-RURAL
                        , :RURCB000.CPROG-RURAL
             FROM   DB2PRD.ORIGEM_RECURSO
             WHERE CORIGE_REC         = :RURCB000.CORIGE-REC
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'ORIGEM_RECURSO   ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3010-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3020-SELECT-RURCB014        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CMUN_REFT_BACEN
                  INTO    :RURCB014.CMUN-REFT-BACEN
             FROM   DB2PRD.MUNICIPIO_RURAL
             WHERE CMUN_RURAL         = :RURCB014.CMUN-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'MUNICIPIO_RURAL  ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0030'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3020-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3030-SELECT-RURCB018        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CTPO_GARNT_RECOR
                  INTO    :RURCB018.CTPO-GARNT-RECOR
             FROM   DB2PRD.ESPCE_TITLO_GARNT
             WHERE CESPCE_TITLO_GARNT = :RURCB018.CESPCE-TITLO-GARNT
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'ESPCE_TITLO_GARNT' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0040'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3030-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3035-SELECT-RURCB022        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CCATEG_PROTR_RECOR
                  INTO    :RURCB022.CCATEG-PROTR-RECOR
             FROM   DB2PRD.TPO_PROTR_RURAL
             WHERE CTPO_PROTR_RURAL   = :RURCB022.CTPO-PROTR-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPO_PROTR_RURAL  ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0041'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                EQUAL  +100
               MOVE '5555'            TO CCATEG-PROTR-RECOR OF RURCB022
           END-IF.

      *---------------------------------------------------------------*
       3035-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3040-SELECT-RURCB028        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CPLANO_REFIN_RECOR
                  INTO    :RURCB028.CPLANO-REFIN-RECOR
             FROM   DB2PRD.PLANO_REFIN
             WHERE CPLANO_REFIN       = :RURCB028.CPLANO-REFIN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PLANO_REFIN      ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0050'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3040-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3050-SELECT-RUECV012-JUROS  SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR          TO CCONTR         OF RUECV012.
           MOVE 8200                   TO WRK-INI2.
           MOVE 8299                   TO WRK-FIM2.

           EXEC SQL
             SELECT VEVNTO_RURAL
                  INTO    :RUECV012.VEVNTO-RURAL
             FROM   DB2PRD.V01EVNTO_RURAL
             WHERE CCONTR             = :RUECV012.CCONTR
               AND (CTPO_EVNTO_RURAL BETWEEN :WRK-INI2 AND
                                             :WRK-FIM2)
                 FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'EVNTO_RURAL      ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0060'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE VEVNTO-RURAL OF RUECV012 TO WRK-TAXA-JUROS.

      *---------------------------------------------------------------*
       3050-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3060-CHAMADA-MESU           SECTION.
      *---------------------------------------------------------------*

           IF  RUEC62-PROCESS  EQUAL  'B'
               GO  TO  3060-50-MESU9116
           END-IF.

           INITIALIZE  WRK-MESU9032
                       WRK-POOL7100-MESU9032
                       WRK-SQLCA-MESU9032
                       WRK-AMBIENTE-MESU9032
                       WRK-IO-PCB-MESU9032
                       WRK-ALT-PCB-MESU9032.
           MOVE 'MESU9032'         TO  WRK-MODULO.
           MOVE CJUNC-DEPDC OF RUFIB002  TO WRK-AGENCIA.
           MOVE 'O'                TO  WRK-AMBIENTE-MESU9032.
           CALL WRK-MODULO  USING WRK-MESU9032
                                  WRK-POOL7100-MESU9032
                                  WRK-SQLCA-MESU9032
                                  WRK-AMBIENTE-MESU9032
                                  WRK-IO-PCB-MESU9032
                                  WRK-ALT-PCB-MESU9032.

           IF  WRK-COD-RETORNO  NOT  EQUAL  ZEROS
               MOVE 03                  TO RUEC62-COD-RET
               MOVE 'AGENCIA INEXISTENTE NO MESU9032'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE WRK-LOJA             TO CFLIAL-CNPJ-ENTID.
           MOVE WRK-CONTROLE         TO WRK-CTRL-CHAR.
           MOVE WRK-CTRL-NUM         TO CCTRL-CNPJ-ENTID.

           GO  TO  3060-99-FIM.

      *---------------------------------------------------------------*
       3060-50-MESU9116.
      *---------------------------------------------------------------*

           INITIALIZE  COMU-DADOS-9016.
           MOVE 'MESU9116'               TO WRK-MODULO.
           MOVE 237                      TO COMU-COD-EMPR
           MOVE CJUNC-DEPDC OF RUFIB002  TO COMU-COD-DEPDC
           MOVE 'VRS001'                 TO COMU-VERSAO
           MOVE SPACES                   TO COMU-MENSAGEM

           CALL    WRK-MODULO     USING     COMU-VERSAO
                                            COMU-MENSAGEM
                                            COMU-DADOS-ENTRADA
                                            COMU-DADOS-SAIDA.
           IF  RETURN-CODE  NOT  EQUAL  ZEROS
               MOVE 03                  TO RUEC62-COD-RET
               MOVE 'AGENCIA INEXISTENTE NO MESU9116'
                                        TO RUEC62-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE COMU-NUM-LOJ-CGC     TO CFLIAL-CNPJ-ENTID.
           MOVE COMU-NUM-CONT-CGC    TO WRK-CTRL-CHAR.
           MOVE WRK-CTRL-NUM         TO CCTRL-CNPJ-ENTID.

      *---------------------------------------------------------------*
       3060-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3070-SELECT-RUECV012-DVCTO  SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR          TO CCONTR         OF RUECV012.
           MOVE 6000                   TO WRK-INI3.
           MOVE 6999                   TO WRK-FIM3.

           EXEC SQL
             SELECT MAX (DEVNTO)
             INTO :RUECV012.DEVNTO
             FROM   DB2PRD.V01EVNTO_RURAL
             WHERE CCONTR             = :RUECV012.CCONTR
               AND (CTPO_EVNTO_RURAL BETWEEN :WRK-INI3 AND
                                             :WRK-FIM3)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'EVNTO_RURAL      ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0070'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE DEVNTO       OF RUECV012 TO WRK-DVCTO-CONTR.

      *---------------------------------------------------------------*
       3070-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-GERA-CONTRATO          SECTION.
      *---------------------------------------------------------------*

       3100-10-INSERT-RUECB09B.

           MOVE ZEROS              TO NENVIO-RECOR       OF RUECB09B.
           MOVE RUEC62-CCONTR      TO CCONTR-CREDT-RURAL OF RUECB09B.
           EXEC SQL
             SELECT  MAX (NENVIO_RECOR)
               INTO :RUECB09B.NENVIO-RECOR
             FROM  DB2PRD.THIST_ENVIO_RECOR
               WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF (SQLCODE           NOT EQUAL ZEROS AND -305 AND -310) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0080'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           ADD  1                  TO NENVIO-RECOR       OF RUECB09B.
           MOVE 'COR0001'          TO CMSGEM-RECOR       OF RUECB09B.
           MOVE SPACES             TO CCTRL-RQUIS-FINCR  OF RUECB09B.
           MOVE SPACES             TO CUNIC-MSGEM        OF RUECB09B.
           MOVE SPACES             TO CREFT-RECOR        OF RUECB09B.
           MOVE 1                  TO CSIT-MSGEM         OF RUECB09B.

           IF  RUEC62-USER  EQUAL  SPACES
               MOVE '999999999'    TO CUSUAR-RESP        OF RUECB09B
           ELSE
               MOVE RUEC62-USER    TO CUSUAR-RESP        OF RUECB09B
           END-IF.

           MOVE NENVIO-RECOR OF RUECB09B  TO RUEC62-NENVIO-RECOR.

           EXEC SQL
             INSERT INTO DB2PRD.THIST_ENVIO_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , HULT_ATULZ
                  , CMSGEM_RECOR
                  , HINCL
                  , CCTRL_RQUIS_FINCR
                  , CUNIC_MSGEM
                  , CREFT_RECOR
                  , CSIT_MSGEM
                  , CUSUAR_RESP)
             VALUES
                   (:RUECB09B.CCONTR-CREDT-RURAL
                  , :RUECB09B.NENVIO-RECOR
                  , CURRENT TIMESTAMP
                  , :RUECB09B.CMSGEM-RECOR
                  , CURRENT TIMESTAMP
                  , :RUECB09B.CCTRL-RQUIS-FINCR
                  , :RUECB09B.CUNIC-MSGEM
                  , :RUECB09B.CREFT-RECOR
                  , :RUECB09B.CSIT-MSGEM
                  , :RUECB09B.CUSUAR-RESP)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0090'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.


      *---------------------------------------------------------------*
      * INSERIR REGISTRO RUECB099
      *---------------------------------------------------------------*
       3100-20-INSERT-RUECB099.

           MOVE NENVIO-RECOR OF RUECB09B TO NENVIO-RECOR   OF RUECB099.
           MOVE RUEC62-CCONTR        TO CCONTR-CREDT-RURAL OF RUECB099.
           MOVE 60746948             TO CCNPJ-ENTID-RESP.
           MOVE ZEROS                TO CCNPJ-PARTC.
           MOVE ZEROS                TO CFLIAL-CNPJ-PARTC.
           MOVE ZEROS                TO CCTRL-CNPJ-PARTC.
           MOVE '01'                 TO CSIT-OPER-RURAL.
           MOVE WRK-DVCTO-CONTR         TO DVCTO-CONTR     OF RUECB099.
           MOVE CJUNC-DEPDC OF RUFIB002 TO WRK-CJUNC-DEPDC.
           MOVE CCDULA      OF RUFIB002 TO WRK-CCDULA.
           MOVE WRK-CEDULA-RECOR        TO CCDULA-RECOR    OF RUECB099.
           MOVE CTPO-GARNT-RECOR OF RURCB018
                                        TO WRK-CTPO-GARNT-RECOR.
           MOVE WRK-CTPO-GARNT-X        TO CTPO-INSTT-CREDT.
           MOVE CPLANO-REFIN-RECOR      TO CTPO-FONTE-RECOR.

           MOVE CPROG-RURAL OF RURCB000 TO CPROG-RURAL OF RUECB099.

           MOVE CMUN-RURAL  OF RURCB014 TO WRK-CMUN-RURAL9.
           MOVE WRK-CMUN-RURAL6         TO CMUN-REFT-BACEN OF RUECB099.
           MOVE RUEC58-EMPREEND         TO CEMPTO-BACEN.
           MOVE RUEC58-SIST-PROD        TO CSIST-PROD.
           IF  CSIST-PROD(10:02)  EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO CSIST-PROD(10:02)
           END-IF


STE001     IF  RUEC58-COUTRO-TRATO-SOLO
STE001                             GREATER ZEROS
.              PERFORM 3117-ACESSAR-CRUR2G4C
.     **       _OVE CRURWG4I-S-COUTRO-SOLO-BACEN
.     **                           TO CSIST-PROD(12:02)
.              MOVE CRURWG4I-S-COUTRO-SOLO-BACEN
.                                  TO WRK-CTRL-NUM-2
.              MOVE WRK-CTRL-CHAR2 TO CSIST-PROD(12:02)
STE001     END-IF
           MOVE VFINAN OF RUFIB002      TO VPCELA-LIBRC     OF RUECB099.
           MOVE VREC-PPRIO OF RUFIB002  TO VPCELA-REC-PPRIO OF RUECB099.
           MOVE ZEROS                   TO PALIQT-PROAGRO.
           MOVE WRK-TAXA-JUROS-3V5      TO PJURO-ENCAR-FINCR.
           MOVE ZEROS                   TO CTPO-ENCAR-COMPL.
           MOVE ZEROS                   TO CTSORO-NACIO.
           MOVE ZEROS                   TO PRISCO-TSORO-NACIO.
           MOVE ZEROS                   TO PRISCO-FUNDO-CNSTT.

           IF  CUND-MEDD-FINCD  OF RUFIB002  EQUAL  1 OR 25
BR0718                                          OR 54 OR 60
               MOVE QUND-FINCD-RURAL OF RUFIB002 TO WRK-QUND-FINCD11
               MOVE WRK-QUND-FINCD9     TO QAREA-RECOR
               MOVE ZEROS               TO QUND-FINCD-RURAL OF RUECB099
           ELSE
               MOVE ZEROS               TO QAREA-RECOR
               MOVE QUND-FINCD-RURAL OF RUFIB002
                                        TO QUND-FINCD-RURAL OF RUECB099
           END-IF.

           MOVE VFINAN OF RUFIB002      TO VFINAN OF RUECB099.
           MOVE CCATEG-PROTR-RECOR OF RURCB022
                                       TO CCATEG-PROTR-RECOR OF RUECB099
201502
201507     IF  CCTRO-CUSTO             NOT EQUAL 'MASS'
201502     AND CCTCIA-PROD-PROVV       OF RURCB006
201502                                 EQUAL 1
201502         COMPUTE QUND-PROD-PROVV     OF RUECB099
201502         EQUAL   QUND-PROD-PROVV     OF RUFIB002
201502         *       QUND-FINCD-RURAL    OF RUFIB002
201502     ELSE
201502         MOVE QUND-PROD-PROVV  OF RUFIB002
201502                                  TO QUND-PROD-PROVV  OF RUECB099
201502     END-IF.
201502
           MOVE CUND-MEDD-FINCD  OF RUFIB002
                                        TO CUND-MEDD-FINCD  OF RUECB099.
           MOVE CUND-MEDD-PROVV  OF RUFIB002
                                        TO CUND-MEDD-PROVV  OF RUECB099.
           PERFORM  3110-MONTA-SAFRA.
           MOVE WRK-INI-SAFRA           TO DINIC-SAFRA      OF RUECB099.
           MOVE WRK-FIM-SAFRA           TO DFIM-SAFRA       OF RUECB099.

HE0814     IF (RUEC58-EMPREEND(1:4)     EQUAL '1201') OR
HE0814        (RUEC58-EMPREEND(1:4)     EQUAL '2226')
HE0814         MOVE 9                   TO CTPO-GARNT-RECOR OF RUECB099
HE0814     ELSE
HE0814         MOVE 0                   TO CTPO-GARNT-RECOR OF RUECB099
HE0814     END-IF.

EG0617***  REGRA BACEN DE 30/05/2017
EG0617***  DESCONTINUADO CODIGO 0 - NAO SE APLICA. SUBSTITUINDO POR
EG0617***  9 - SEM ADESAO A SEGURO.
EG0617     IF  CTPO-GARNT-RECOR         OF RUECB099
EG0617                                  EQUAL ZEROS
EG0617        MOVE  9                   TO CTPO-GARNT-RECOR OF RUECB099
EG0617     END-IF.
EG0617*
EG0616****-IF  CORIGE-REC OF RUFIB002   EQUAL 28 OR 34
EG0616*****   -MOVE 3                   TO CTPO-GARNT-RECOR OF RUECB099
EG0616****-END-IF.
EG0616
RE1017     PERFORM  3130-VERIFICAR-SEGURO
RE1017     IF CSIT-REG-RURAL  EQUAL 'A'
RE1017        MOVE 3          TO CTPO-GARNT-RECOR OF RUECB099
RE1017     END-IF.

           PERFORM  3120-CALCULA-VRECTA-BRUTA.
           MOVE WRK-VRECTA-BRUTA        TO VRECTA-BRUTA-RECOR.

           EXEC SQL
             INSERT INTO DB2PRD.TCONTR_ENVIO_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , CCNPJ_ENTID_RESP
                  , CFLIAL_CNPJ_ENTID
                  , CCTRL_CNPJ_ENTID
                  , CCNPJ_PARTC
                  , CFLIAL_CNPJ_PARTC
                  , CCTRL_CNPJ_PARTC
                  , CSIT_OPER_RURAL
                  , DCDULA
                  , DVCTO_CONTR
                  , CCDULA_RECOR
                  , CTPO_INSTT_CREDT
                  , VFINAN
                  , CCATEG_PROTR_RECOR
                  , CPROG_RURAL
                  , CTPO_FONTE_RECOR
                  , CMUN_REFT_BACEN
                  , CEMPTO_BACEN
                  , CSIST_PROD
                  , VPCELA_LIBRC
                  , VPCELA_REC_PPRIO
                  , PALIQT_PROAGRO
                  , PJURO_ENCAR_FINCR
                  , CTPO_ENCAR_COMPL
                  , CTSORO_NACIO
                  , PRISCO_TSORO_NACIO
                  , PRISCO_FUNDO_CNSTT
                  , QAREA_RECOR
                  , QUND_FINCD_RURAL
                  , CUND_MEDD_FINCD
                  , QUND_PROD_PROVV
                  , CUND_MEDD_PROVV
                  , DINIC_SAFRA
                  , DFIM_SAFRA
                  , CTPO_GARNT_RECOR
                  , VRECTA_BRUTA_RECOR
                  , DMOVTO_RECOR)
             VALUES
                   (:RUECB099.CCONTR-CREDT-RURAL
                  , :RUECB099.NENVIO-RECOR
                  , :RUECB099.CCNPJ-ENTID-RESP
                  , :RUECB099.CFLIAL-CNPJ-ENTID
                  , :RUECB099.CCTRL-CNPJ-ENTID
                  , :RUECB099.CCNPJ-PARTC
                  , :RUECB099.CFLIAL-CNPJ-PARTC
                  , :RUECB099.CCTRL-CNPJ-PARTC
                  , :RUECB099.CSIT-OPER-RURAL
                  , CURRENT DATE
                  , :RUECB099.DVCTO-CONTR
                  , :RUECB099.CCDULA-RECOR
                  , :RUECB099.CTPO-INSTT-CREDT
                  , :RUECB099.VFINAN
                  , :RUECB099.CCATEG-PROTR-RECOR
                  , :RUECB099.CPROG-RURAL
                  , :RUECB099.CTPO-FONTE-RECOR
                  , :RUECB099.CMUN-REFT-BACEN
                  , :RUECB099.CEMPTO-BACEN
                  , :RUECB099.CSIST-PROD
                  , :RUECB099.VPCELA-LIBRC
                  , :RUECB099.VPCELA-REC-PPRIO
                  , :RUECB099.PALIQT-PROAGRO
                  , :RUECB099.PJURO-ENCAR-FINCR
                  , :RUECB099.CTPO-ENCAR-COMPL
                  , :RUECB099.CTSORO-NACIO
                  , :RUECB099.PRISCO-TSORO-NACIO
                  , :RUECB099.PRISCO-FUNDO-CNSTT
                  , :RUECB099.QAREA-RECOR
                  , :RUECB099.QUND-FINCD-RURAL
                  , :RUECB099.CUND-MEDD-FINCD
                  , :RUECB099.QUND-PROD-PROVV
                  , :RUECB099.CUND-MEDD-PROVV
                  , :RUECB099.DINIC-SAFRA
                  , :RUECB099.DFIM-SAFRA
                  , :RUECB099.CTPO-GARNT-RECOR
                  , :RUECB099.VRECTA-BRUTA-RECOR
                  , CURRENT DATE)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0100'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3110-MONTA-SAFRA            SECTION.
      *---------------------------------------------------------------*

           MOVE  DINIC-SAFRA OF RUFIB002 TO  WRK-SAFRA-DB2.
           MOVE  WRK-SAFRA-DB2(3:4)      TO  WRK-INI-SAFRA.
           MOVE  DFIM-SAFRA OF RUFIB002  TO  WRK-SAFRA-DB2.
           MOVE  WRK-SAFRA-DB2(3:4)      TO  WRK-FIM-SAFRA.
           MOVE  WRK-TIMESTAMP(1:4)      TO  WRK-DATA-DB2(7:4)
           MOVE  WRK-ANO-DB2             TO  WRK-ANO-DEMIS.

      * ***** FINALIDADE = CUSTEIO

           IF  RUEC58-EMPREEND(2:1)  EQUAL  '2'
               MOVE  WRK-ANO-DEMIS       TO  WRK-INI-SAFRA
               IF  WRK-FIM-SAFRA            EQUAL  WRK-INI-SAFRA      OR
                   WRK-FIM-SAFRA            EQUAL  WRK-INI-SAFRA + 1  OR
                   WRK-FIM-SAFRA            EQUAL  WRK-INI-SAFRA + 2
                   CONTINUE
               ELSE
                   MOVE  WRK-INI-SAFRA         TO  WRK-FIM-SAFRA
               END-IF
           END-IF.

      * ***** FINALIDADE = COMERCIALIZACAO

           IF  RUEC58-EMPREEND(2:1)  EQUAL  '1'
               IF  WRK-INI-SAFRA        EQUAL  WRK-ANO-DEMIS      OR
                   WRK-INI-SAFRA        EQUAL  WRK-ANO-DEMIS - 1  OR
                   WRK-INI-SAFRA        EQUAL  WRK-ANO-DEMIS - 2
                   CONTINUE
               ELSE
                   MOVE  WRK-ANO-DEMIS  TO  WRK-INI-SAFRA
               END-IF
           END-IF.

           IF  RUEC58-EMPREEND(2:1)  EQUAL  '1'
               IF  WRK-FIM-SAFRA        EQUAL  WRK-INI-SAFRA      OR
                   WRK-FIM-SAFRA        EQUAL  WRK-INI-SAFRA + 1  OR
                   WRK-FIM-SAFRA        EQUAL  WRK-INI-SAFRA + 2
                   CONTINUE
               ELSE
                   MOVE  WRK-INI-SAFRA     TO  WRK-FIM-SAFRA
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3110-99-FIM.                EXIT.
      *---------------------------------------------------------------*

STEFAB*----------------------------------------------------------------*
.      3117-ACESSAR-CRUR2G4C           SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE WRK-AREA-CRUR2G4C.
.
.          MOVE 'CRURW00W'             TO  CRURW00W-COD-LAYOUT
.                                      OF  WRK-AREA-CRUR2G4C
.          MOVE 27                     TO  CRURW00W-TAM-LAYOUT
.                                      OF  WRK-AREA-CRUR2G4C
.          MOVE 'CRURWG4I'             TO  CRURWG4I-COD-LAYOUT
.                                      OF  WRK-AREA-CRUR2G4C
.          MOVE 296                    TO  CRURWG4I-TAM-LAYOUT
.                                      OF  WRK-AREA-CRUR2G4C
.          MOVE RUEC58-COUTRO-TRATO-SOLO
.                                      TO  WRK-MANEJO
.          MOVE WRK-MANEJO-N           TO  CRURWG4I-E-COUTRO-TRATO-SOLO
.
.          CALL WRK-CRUR2G4C           USING WRK-AREA-CRUR2G4C.
.
.          IF  CRURW00W-COD-RETORNO    OF  WRK-AREA-CRUR2G4C
.                                      NOT EQUAL ZEROS
.              MOVE  ZEROS             TO  CRURWG4I-S-COUTRO-SOLO-BACEN
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3117-99-FIM.    EXIT.
STEFAB*----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3120-CALCULA-VRECTA-BRUTA   SECTION.
      *---------------------------------------------------------------*

           MOVE VRECTA-BRUTA-EMPTO OF RUFIB002 TO WRK-VRECTA-BRUTA.

           IF  CCTRO-CUSTO  EQUAL  'MASS'
               MOVE  VFINAN OF RUFIB002 TO WRK-VRECTA-BRUTA
               GO  TO  3120-99-FIM
MF0215     ELSE
MF0215         GO  TO  3120-99-FIM
           END-IF.

MF0215* ******* RECEITA BRUTA DEVE CONSIDERAR O CALCULADO NO
MF0215* ******* RUEC7000 E/OU RUEC8000

           IF  RUEC58-EMPREEND(1:2)  NOT EQUAL  '12'
               GO  TO  3120-99-FIM
           END-IF.

           MOVE RUEC62-CCONTR        TO CCONTR             OF RUECB007.
           EXEC SQL
               SELECT  CCLI_UNIC_BDSCO
                 INTO :RUECB007.CCLI-UNIC-BDSCO
               FROM    DB2PRD.CEDULA_TEMPR
               WHERE   CCONTR           = :RUECB007.CCONTR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0110'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CCLI-UNIC-BDSCO OF RUECB007 TO CCLUB    OF RUECB063.

           EXEC SQL
             SELECT  HULT_EFTVC_ALT
             INTO  :RUECB063.HULT-EFTVC-ALT
             FROM   DB2PRD.TRSUMO_CLI_RURAL
             WHERE CCLUB              = :RUECB063.CCLUB
               AND CSIT_CAD_CLI       = 'V'
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRSUMO_CLI_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0120'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               GO  TO  3120-99-FIM
           END-IF.

           MOVE CCLI-UNIC-BDSCO OF RUECB007 TO CCLUB    OF RUECB061.
           MOVE HULT-EFTVC-ALT  OF RUECB063 TO
                                       HULT-ATULZ-EFETU OF RUECB061.
           MOVE CPRODT-RURAL    OF RUFIB002 TO CPRODT-RURAL OF RUECB061.

           EXEC SQL
               SELECT  VRENDA_AGROP
                 INTO :RUECB061.VRENDA-AGROP
               FROM    DB2PRD.TRENDA_AGROP_PROTR
               WHERE   CCLUB            = :RUECB061.CCLUB
                 AND   HULT_ATULZ_EFETU < :RUECB061.HULT-ATULZ-EFETU
                 AND   CPRODT_RURAL     = :RUECB061.CPRODT-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TRENDA_AGROP_PROTR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0130'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               GO  TO  3120-99-FIM
           END-IF.

      *---------------------------------------------------------------*
      * CALCULO
      *---------------------------------------------------------------*
       3120-20-CALCULO.

      * ***** ATIVIDADE = AGRICOLA

           IF  RUEC58-EMPREEND(1:2)  EQUAL  '12'
               COMPUTE WRK-VRECTA-BRUTA = QUND-FINCD-RURAL OF RUFIB002 *
                  QUND-PROD-PROVV OF RUFIB002 * VRENDA-AGROP OF RUECB061
           END-IF.

      *---------------------------------------------------------------*
       3120-99-FIM.                EXIT.
      *---------------------------------------------------------------*

RE1017*---------------------------------------------------------------*
RE1017 3130-VERIFICAR-SEGURO       SECTION.
RE1017*---------------------------------------------------------------*
RE1017
RE1017     MOVE CJUNC-DEPDC OF RUFIB002 TO CJUNC-DEPDC OF RUECB09J
RE1017     MOVE CCDULA      OF RUFIB002 TO CCDULA      OF RUECB09J
RE1017
RE1017     EXEC SQL
RE1017         SELECT  CSIT_REG_RURAL
RE1017           INTO :RUECB09J.CSIT-REG-RURAL
RE1017         FROM    DB2PRD.TAPOLC_SEGUR_OBRIG
RE1017         WHERE   CJUNC_DEPDC      = :RUECB09J.CJUNC-DEPDC
RE1017           AND   CCDULA           = :RUECB09J.CCDULA
RE1017     END-EXEC.
RE1017
RE1017     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
RE1017        (SQLWARN0                 EQUAL 'W')
RE1017         MOVE 99                  TO RUEC62-COD-RET
RE1017         MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
RE1017         MOVE 'DB2'               TO ERR-TIPO-ACESSO
RE1017         MOVE 'TAPOLC_SEGUR_OBRIG'
RE1017                                  TO ERR-DBD-TAB
RE1017         MOVE 'SELECT'            TO ERR-FUN-COMANDO
RE1017         MOVE SQLCODE             TO ERR-SQL-CODE
RE1017         MOVE '3130'              TO ERR-LOCAL
RE1017         MOVE SPACES              TO ERR-SEGM
RE1017         PERFORM 4000-ROTINA-FIM
RE1017     END-IF.
RE1017
RE1017     IF  SQLCODE                  EQUAL +100
RE1017         MOVE SPACES              TO CSIT-REG-RURAL OF RUECB09J
RE1017     END-IF.
RE1017
RE1017*---------------------------------------------------------------*
RE1017 3130-99-FIM.                EXIT.
RE1017*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-GERA-PARCELAS          SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR      TO CCONTR            OF RUECV012.
           MOVE ZEROS              TO WRK-NPCELA-CONTR.
           MOVE  6000              TO WRK-INI1.
           MOVE  6999              TO WRK-FIM1.

           EXEC SQL
               OPEN CSR01-RUECV012
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'EVNTO_RURAL      ' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0140'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
      * SELECIONAR
      *---------------------------------------------------------------*
       3200-20-FETCH.

           EXEC SQL
                FETCH    CSR01-RUECV012
                  INTO    :RUECV012.CTPO-EVNTO-RURAL
                        , :RUECV012.DEVNTO
                        , :RUECV012.VEVNTO-RURAL
                        , :RUECV012.VPCELA-REC-PPRIO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'EVNTO_RURAL      ' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0150'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF (SQLCODE                  EQUAL +100)
               GO  TO  3200-90-CLOSE
           END-IF.

           ADD  1                      TO WRK-NPCELA-CONTR
           MOVE WRK-NPCELA-CONTR       TO NPCELA-CONTR
           MOVE CCONTR OF RUFIB002     TO CCONTR-CREDT-RURAL OF RUECB09D
           MOVE NENVIO-RECOR OF RUECB099  TO NENVIO-RECOR    OF RUECB09D
           MOVE DEVNTO                 TO DVCTO-PCELA-CONTR
           MOVE VEVNTO-RURAL           TO VPCELA-RURAL

           EXEC SQL
             INSERT INTO DB2PRD.TPCELA_ENVIO_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , NPCELA_CONTR
                  , DVCTO_PCELA_CONTR
                  , VPCELA_RURAL)
             VALUES
                   (:RUECB09D.CCONTR-CREDT-RURAL
                  , :RUECB09D.NENVIO-RECOR
                  , :RUECB09D.NPCELA-CONTR
                  , :RUECB09D.DVCTO-PCELA-CONTR
                  , :RUECB09D.VPCELA-RURAL)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0160'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           GO  TO  3200-20-FETCH.

      *---------------------------------------------------------------*
      * FECHAR CURSOR CSR01-RUECV012
      *---------------------------------------------------------------*
       3200-90-CLOSE.

           EXEC SQL
               CLOSE CSR01-RUECV012
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'EVNTO_RURAL      ' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0170'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3200-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-GERA-PESSOAS           SECTION.
      *---------------------------------------------------------------*

EG0617     MOVE ZEROS                  TO WRK-NPSSOA-CONTR
EG0617     PERFORM 3500-PROPRIETARIOS-IMOVEIS.
EG0617
EG0617     MOVE ZEROS                  TO CCONTR            OF RUECB09E
EG0617     MOVE ZEROS                  TO CSEQ-MATR-RURAL   OF RUECB09E.
EG0617
EG0617     MOVE RUEC62-CCONTR          TO CCONTR          OF RUECB014
EG0617     GO TO 3300-40-EMITENTE.
EG0617
      *-- PROPRIETARIO --**
       3300-10-PROPRIETARIOS.

           MOVE 'P'                    TO CTPO-RESP-PSSOA OF RUECB014
           MOVE ZEROS                  TO WRK-NPSSOA-CONTR
           MOVE RUEC62-CCONTR          TO CCONTR          OF RUECB014

           EXEC SQL
               OPEN CSR01-RUECB014
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0180'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
      * SELECIOANAR
      *---------------------------------------------------------------*
       3300-20-FETCH.

           EXEC SQL
                FETCH    CSR01-RUECB014
                  INTO    :RUECB014.CTPO-PSSOA
                        , :RUECB014.CTPO-RESP-PSSOA
                        , :RUECB014.CCGC-CPF
                        , :RUECB014.CFLIAL-CGC
                        , :RUECB014.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0190'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF (SQLCODE                  EQUAL +100)
               GO  TO  3300-30-CLOSE
           END-IF.

           PERFORM  3310-INSERT-RUECB09E.
           GO  TO  3300-20-FETCH.

      *---------------------------------------------------------------*
      * FECHAR CURSOR CSR01-RUECB014
      *---------------------------------------------------------------*
       3300-30-CLOSE.

           EXEC SQL
               CLOSE CSR01-RUECB014
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0200'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
      * SELECIONAR
      *---------------------------------------------------------------*
       3300-40-EMITENTE.

           EXEC SQL
             SELECT CTPO_PSSOA
                  , CTPO_RESP_PSSOA
                  , CCGC_CPF
                  , CFLIAL_CGC
                  , CCTRL_CPF_CGC
                  INTO    :RUECB014.CTPO-PSSOA
                        , :RUECB014.CTPO-RESP-PSSOA
                        , :RUECB014.CCGC-CPF
                        , :RUECB014.CFLIAL-CGC
                        , :RUECB014.CCTRL-CPF-CGC
             FROM   DB2PRD.PESSOA_CONTR_RURAL
             WHERE CCONTR             = :RUECB014.CCONTR
               AND CTPO_RESP_PSSOA    = 'T'
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_RURAL      ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0210'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           PERFORM  3310-INSERT-RUECB09E.
EG0617****-IF  WRK-NPSSOA-CONTR  EQUAL  1
EG0617****    -MOVE 'P'                 TO CTPO-RESP-PSSOA OF RUECB014
EG0617****    -PERFORM  3310-INSERT-RUECB09E
EG0617****-END-IF.

      *---------------------------------------------------------------*
       3300-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3310-INSERT-RUECB09E        SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR OF RUFIB002     TO CCONTR-CREDT-RURAL OF RUECB09E
           MOVE NENVIO-RECOR OF RUECB099  TO NENVIO-RECOR    OF RUECB09E
EG0617
EG0617     IF  CTPO-RESP-PSSOA         OF RUECB09E NOT EQUAL 'P'
EG0617         MOVE CTPO-RESP-PSSOA    OF RUECB014
EG0617                                 TO CTPO-RESP-PSSOA    OF RUECB09E
EG0617         IF  CTPO-RESP-PSSOA     OF RUECB014 EQUAL 'T'
EG0617             MOVE 'E'            TO CTPO-RESP-PSSOA OF RUECB09E
EG0617         END-IF
EG0617     END-IF.
EG0617
           MOVE CTPO-PSSOA      OF RUECB014 TO
                                          CTPO-PSSOA         OF RUECB09E
           MOVE CCGC-CPF        OF RUECB014 TO
                                          CCNPJ-CPF          OF RUECB09E
           MOVE CFLIAL-CGC      OF RUECB014 TO
                                          CFLIAL-CNPJ        OF RUECB09E
           MOVE CCTRL-CPF-CGC   OF RUECB014 TO
                                          CCTRL-CNPJ-CPF     OF RUECB09E
EG0617
EG0617     PERFORM 3311-INSERT-RUECB09E.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3310-99-FIM.                EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3311-INSERT-RUECB09E        SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     IF  WRK-NPSSOA-CONTR        NOT NUMERIC
EG0617         MOVE ZEROS              TO WRK-NPSSOA-CONTR
EG0617     END-IF.
EG0617
EG0617     ADD  1                      TO WRK-NPSSOA-CONTR
EG0617     MOVE WRK-NPSSOA-CONTR       TO NPSSOA-CONTR.
EG0617
EG0617     IF  CCONTR                  OF RUECB09E NOT NUMERIC
EG0617     OR  CCONTR                  OF RUECB09E EQUAL ZEROS
RG0617     OR  CSEQ-MATR-RURAL         OF RUECB09E NOT NUMERIC
RG0617     OR  CSEQ-MATR-RURAL         OF RUECB09E EQUAL ZEROS
EG0617         MOVE -1                 TO WRK-B040-CCONTR-N
EG0617         MOVE -1                 TO WRK-B040-CSEQ-MATR-RURAL-N
EG0617     ELSE
EG0617         MOVE ZEROS              TO WRK-B040-CCONTR-N
EG0617         MOVE ZEROS              TO WRK-B040-CSEQ-MATR-RURAL-N
EG0617     END-IF.
EG0617
           EXEC SQL
             INSERT INTO DB2PRD.TPSSOA_ENVIO_RECOR
             ( CCONTR_CREDT_RURAL
             , NENVIO_RECOR
             , NPSSOA_CONTR
             , CTPO_RESP_PSSOA
             , CTPO_PSSOA
             , CCNPJ_CPF
             , CFLIAL_CNPJ
             , CCTRL_CNPJ_CPF
EG0617       , CCONTR
EG0617       , CSEQ_MATR_RURAL )
             VALUES
             ( :RUECB09E.CCONTR-CREDT-RURAL
             , :RUECB09E.NENVIO-RECOR
             , :RUECB09E.NPSSOA-CONTR
             , :RUECB09E.CTPO-RESP-PSSOA
             , :RUECB09E.CTPO-PSSOA
             , :RUECB09E.CCNPJ-CPF
             , :RUECB09E.CFLIAL-CNPJ
             , :RUECB09E.CCTRL-CNPJ-CPF
EG0617       , :RUECB09E.CCONTR          :WRK-B040-CCONTR-N
EG0617       , :RUECB09E.CSEQ-MATR-RURAL :WRK-B040-CSEQ-MATR-RURAL-N )
           END-EXEC.

EG0617     IF (SQLCODE                  NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0220'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

EG0617     MOVE SPACES                 TO CTPO-RESP-PSSOA OF RUECB09E.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3311-99-FIM.                EXIT.
EG0617*---------------------------------------------------------------*
EG0617
      *---------------------------------------------------------------*
       3400-GERA-PRODUTOS-CONSORC  SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC62-CCONTR          TO CCONTR-CREDT-RURAL OF RUECB09G

           EXEC SQL
               OPEN CSR01-RUECB09G
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL-CSCDO' TO ERR-DBD-TAB
               MOVE 'OPEN  '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0230'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
      * SELECIONAR
      *---------------------------------------------------------------*
       3400-20-FETCH.

           EXEC SQL
                FETCH    CSR01-RUECB09G
                  INTO    :RUECB09G.CPRODT-RURAL
                          :WRK-CPRODT-RURAL
                        , :RUECB09G.NPRODT-RURAL-CSCDO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_CSCDO' TO ERR-DBD-TAB
               MOVE 'FETCH '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0240'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF (SQLCODE                  EQUAL +100)
               GO  TO  3400-90-CLOSE
           END-IF.

           IF  WRK-CPRODT-RURAL  LESS  ZEROS
               MOVE ZEROS TO CPRODT-RURAL OF RUECB09G
           END-IF.

           MOVE CCONTR OF RUFIB002     TO CCONTR-CREDT-RURAL OF RUECB09F
           MOVE NENVIO-RECOR OF RUECB099  TO NENVIO-RECOR    OF RUECB09F
           MOVE NPRODT-RURAL-CSCDO OF RUECB09G
                                       TO NPRODT-RURAL-RECOR OF RUECB09F
           MOVE CPRODT-RURAL OF RUECB09G  TO CPRODT-RURAL    OF RUECB09F

           EXEC SQL
             INSERT INTO DB2PRD.TPRODT_RURAL_RECOR
                   (CCONTR_CREDT_RURAL
                  , NENVIO_RECOR
                  , NPRODT_RURAL_RECOR
                  , CPRODT_RURAL)
             VALUES
                   (:RUECB09F.CCONTR-CREDT-RURAL
                  , :RUECB09F.NENVIO-RECOR
                  , :RUECB09F.NPRODT-RURAL-RECOR
                  , :RUECB09F.CPRODT-RURAL)
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR' TO ERR-DBD-TAB
               MOVE 'INSERT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0250'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           GO  TO  3400-20-FETCH.

      *---------------------------------------------------------------*
      * FECHAR CURSOR CSR01-RUECB09G
      *---------------------------------------------------------------*
       3400-90-CLOSE.

           EXEC SQL
               CLOSE CSR01-RUECB09G
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_CSCDO' TO ERR-DBD-TAB
               MOVE 'CLOSE '            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0260'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3400-99-FIM.                EXIT.
      *---------------------------------------------------------------*

EG0617*---------------------------------------------------------------*
EG0617 3500-PROPRIETARIOS-IMOVEIS      SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     MOVE CCONTR                 OF RUFIB002
EG0617                                 TO CCONTR OF RUECB040.
EG0617
EG0617     PERFORM 3510-OPEN-CSR01-RUECB040.
EG0617
EG0617     PERFORM 3520-FETCH-CSR01-RUECB040.
EG0617
EG0617     PERFORM 3540-PROCESSAR-CSR01
EG0617     UNTIL   WRK-FIM-B040        EQUAL 'S'.
EG0617
EG0617     PERFORM 3530-CLOSE-CSR01-RUECB040.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3500-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3510-OPEN-CSR01-RUECB040        SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       OPEN CSR01-RUECB040
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
EG0617         MOVE 'OPEN'             TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3510'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617     MOVE 'N'                    TO WRK-FIM-B040.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3510-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3520-FETCH-CSR01-RUECB040       SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       FETCH CSR01-RUECB040
EG0617       INTO
EG0617         :RUECB040.CSEQ-IMOV-RURAL
EG0617       , :RUECB040.CCONTR
EG0617       , :RUECB040.CSEQ-MATR-RURAL
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
EG0617         MOVE 'FETCH'            TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3520'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617     IF  SQLCODE                 EQUAL +100
EG0617         MOVE 'S'                TO WRK-FIM-B040
EG0617     END-IF.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3520-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3530-CLOSE-CSR01-RUECB040       SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       CLOSE CSR01-RUECB040
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
EG0617         MOVE 'CLOSE'            TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3530'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3530-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3540-PROCESSAR-CSR01            SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     MOVE RUEC62-CCONTR        TO CCONTR             OF RUECB007.
EG0617
EG0617     EXEC SQL
EG0617         SELECT  VALUE ( MIN ( CCLI_UNIC_BDSCO ) , 0 )
EG0617           INTO :RUECB007.CCLI-UNIC-BDSCO
EG0617         FROM    DB2PRD.CEDULA_TEMPR
EG0617         WHERE   CCONTR           = :RUECB007.CCONTR
EG0617     END-EXEC.
EG0617
EG0617     IF (SQLCODE                  NOT EQUAL ZEROS) OR
EG0617        (SQLWARN0                 EQUAL 'W')
EG0617         MOVE 99                  TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'               TO ERR-TIPO-ACESSO
EG0617         MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
EG0617         MOVE 'SELECT'            TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE             TO ERR-SQL-CODE
EG0617         MOVE '354A'              TO ERR-LOCAL
EG0617         MOVE SPACES              TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
BR1119
BR1119     IF  CCLI-UNIC-BDSCO EQUAL  ZEROS AND
BR1119         CCTRO-CUSTO OF RUFIB002 EQUAL 'MASS'
BR1119         PERFORM 9999-CARREGAR-FUMAGEIRA
BR1119         PERFORM 3311-INSERT-RUECB09E
BR1119         PERFORM 3520-FETCH-CSR01-RUECB040
BR1119         GO TO 3540-99-FIM
BR1119     END-IF
EG0617
EG0617     MOVE CCLI-UNIC-BDSCO        OF RUECB007
EG0617                                 TO CCLUB           OF RUECB056.
EG0617     MOVE CSEQ-IMOV-RURAL        OF RUECB040
EG0617                                 TO CSEQ-IMOV-RURAL OF RUECB056.
EG0617     MOVE ZEROS                  TO CSEQ-ATULZ-IMOV OF RUECB056.
EG0617
EG0617     EXEC SQL
EG0617       SELECT RTPO_EXPLO_IMOV
EG0617       INTO   :RUECB056.RTPO-EXPLO-IMOV
EG0617       FROM   DB2PRD.TIMOV_RURAL_EXPDO
EG0617       WHERE  CCLUB              = :RUECB056.CCLUB
EG0617       AND    CSEQ_IMOV_RURAL    = :RUECB056.CSEQ-IMOV-RURAL
EG0617       AND    CSEQ_ATULZ_IMOV    = :RUECB056.CSEQ-ATULZ-IMOV
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'TIMOV_RURAL_EXPDO' TO ERR-DBD-TAB
EG0617         MOVE 'SELECT'           TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3540'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617     IF  SQLCODE                 EQUAL +100
EG0617         PERFORM 3520-FETCH-CSR01-RUECB040
EG0617         GO TO 3540-99-FIM
EG0617     END-IF.
EG0617
EG0617     IF  RTPO-EXPLO-IMOV         OF RUECB056
EG0617                                 EQUAL 'PROPRIA' OR 'CONDOMINIO'
EG0617         PERFORM 3541-GERAR-PROPR-TITULAR
EG0617     END-IF
EG0617
EG0617     IF  RTPO-EXPLO-IMOV         OF RUECB056 EQUAL 'PROPRIA'
EG0617         PERFORM 3520-FETCH-CSR01-RUECB040
EG0617         GO TO 3540-99-FIM
EG0617     END-IF.
EG0617
EG0617     MOVE CCLI-UNIC-BDSCO        OF RUECB007
EG0617                                 TO CCLUB              OF RUECB059
EG0617     MOVE CSEQ-IMOV-RURAL        OF RUECB040
EG0617                                 TO CSEQ-IMOV-RURAL    OF RUECB059
EG0617     MOVE ZEROS                  TO CSEQ-ATULZ-IMOV    OF RUECB059
EG0617
EG0617     PERFORM 3550-OPEN-CSR01-RUECB059.
EG0617
EG0617     PERFORM 3560-FETCH-CSR01-RUECB059.
EG0617
EG0617     PERFORM UNTIL WRK-FIM-B059  EQUAL 'S'
EG0617         PERFORM 3580-GERAR-PROPR-OUTRO
EG0617         PERFORM 3560-FETCH-CSR01-RUECB059
EG0617     END-PERFORM.
EG0617
EG0617     PERFORM 3570-CLOSE-CSR01-RUECB059.
EG0617
EG0617     PERFORM 3520-FETCH-CSR01-RUECB040.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3540-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3541-GERAR-PROPR-TITULAR        SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     MOVE CCONTR                 OF RUFIB002
EG0617                                 TO CCONTR             OF RUECB014
EG0617
EG0617     EXEC SQL
EG0617       SELECT CTPO_PSSOA
EG0617       ,      CTPO_RESP_PSSOA
EG0617       ,      CCGC_CPF
EG0617       ,      CFLIAL_CGC
EG0617       ,      CCTRL_CPF_CGC
EG0617       INTO   :RUECB014.CTPO-PSSOA
EG0617       ,      :RUECB014.CTPO-RESP-PSSOA
EG0617       ,      :RUECB014.CCGC-CPF
EG0617       ,      :RUECB014.CFLIAL-CGC
EG0617       ,      :RUECB014.CCTRL-CPF-CGC
EG0617       FROM   DB2PRD.PESSOA_CONTR_RURAL
EG0617       WHERE  CCONTR             = :RUECB014.CCONTR
EG0617       AND    CTPO_RESP_PSSOA    = 'T'
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'PSSOA_RURAL'      TO ERR-DBD-TAB
EG0617         MOVE 'SELECT'           TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3541'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617     IF  SQLCODE                 EQUAL +100
EG0617         GO TO 3541-99-FIM
EG0617     END-IF.
EG0617
EG0617     MOVE CCONTR                 OF RUFIB002
EG0617                                 TO CCONTR-CREDT-RURAL OF RUECB09E
EG0617     MOVE NENVIO-RECOR           OF RUECB099
EG0617                                 TO NENVIO-RECOR       OF RUECB09E
EG0617     MOVE 'P'                    TO CTPO-RESP-PSSOA    OF RUECB09E
EG0617     MOVE CTPO-PSSOA OF RUECB014 TO CTPO-PSSOA         OF RUECB09E
EG0617     MOVE CCGC-CPF               OF RUECB014
EG0617                                 TO CCNPJ-CPF          OF RUECB09E
EG0617     MOVE CFLIAL-CGC             OF RUECB014
EG0617                                 TO CFLIAL-CNPJ        OF RUECB09E
EG0617     MOVE CCTRL-CPF-CGC          OF RUECB014
EG0617                                 TO CCTRL-CNPJ-CPF     OF RUECB09E
EG0617
EG0617     MOVE CCONTR                 OF RUECB040
EG0617                                 TO CCONTR             OF RUECB09E
EG0617     MOVE CSEQ-MATR-RURAL        OF RUECB040
EG0617                                 TO CSEQ-MATR-RURAL    OF RUECB09E
EG0617
EG0617     PERFORM 3310-INSERT-RUECB09E.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3541-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3550-OPEN-CSR01-RUECB059        SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       OPEN CSR01-RUECB059
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'TPPRIE_IMOV_ARREN' TO ERR-DBD-TAB
EG0617         MOVE 'OPEN'             TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3550'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617     MOVE 'N'                    TO WRK-FIM-B059.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3550-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3560-FETCH-CSR01-RUECB059       SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       FETCH CSR01-RUECB059
EG0617       INTO
EG0617         :RUECB059.CCGC-CPF
EG0617       , :RUECB059.CFLIAL-CGC
EG0617       , :RUECB059.CCTRL-CGC-CPF
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'TPPRIE_IMOV_ARREN' TO ERR-DBD-TAB
EG0617         MOVE 'FETCH'            TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3560'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617     IF  SQLCODE                 EQUAL +100
EG0617         MOVE 'S'                TO WRK-FIM-B059
EG0617     END-IF.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3560-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3570-CLOSE-CSR01-RUECB059       SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     EXEC SQL
EG0617       CLOSE CSR01-RUECB059
EG0617     END-EXEC.
EG0617
EG0617     IF  (SQLCODE                NOT EQUAL ZEROS)
EG0617     OR  (SQLWARN0               EQUAL 'W')
EG0617         MOVE 99                 TO RUEC62-COD-RET
EG0617         MOVE 'ERRO ACESSO DB2'  TO RUEC62-MENSAGEM
EG0617         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG0617         MOVE 'TPPRIE_IMOV_ARREN' TO ERR-DBD-TAB
EG0617         MOVE 'CLOSE'            TO ERR-FUN-COMANDO
EG0617         MOVE SQLCODE            TO ERR-SQL-CODE
EG0617         MOVE '3570'             TO ERR-LOCAL
EG0617         MOVE SPACES             TO ERR-SEGM
EG0617         PERFORM 4000-ROTINA-FIM
EG0617     END-IF.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3570-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3580-GERAR-PROPR-OUTRO          SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     MOVE CCONTR                 OF RUFIB002
EG0617                                 TO CCONTR-CREDT-RURAL OF RUECB09E
EG0617     MOVE NENVIO-RECOR           OF RUECB099
EG0617                                 TO NENVIO-RECOR       OF RUECB09E
EG0617     MOVE 'P'                    TO CTPO-RESP-PSSOA    OF RUECB09E
EG0617
EG0617     IF  CFLIAL-CGC OF RUECB059  EQUAL ZEROS
EG0617         MOVE 1                  TO CTPO-PSSOA         OF RUECB09E
EG0617     ELSE
EG0617         MOVE 2                  TO CTPO-PSSOA         OF RUECB09E
EG0617     END-IF.
EG0617
EG0617     MOVE CCGC-CPF               OF RUECB059
EG0617                                 TO CCNPJ-CPF          OF RUECB09E
EG0617     MOVE CFLIAL-CGC             OF RUECB059
EG0617                                 TO CFLIAL-CNPJ        OF RUECB09E
EG0617     MOVE CCTRL-CGC-CPF          OF RUECB059
EG0617                                 TO CCTRL-CNPJ-CPF     OF RUECB09E
EG0617
EG0617     MOVE CCONTR                 OF RUECB040
EG0617                                 TO CCONTR             OF RUECB09E
EG0617     MOVE CSEQ-MATR-RURAL        OF RUECB040
EG0617                                 TO CSEQ-MATR-RURAL    OF RUECB09E
EG0617
EG0617     PERFORM 3311-INSERT-RUECB09E.
EG0617
EG0617*---------------------------------------------------------------*
EG0617 3580-99-FIM.                    EXIT.
EG0617*---------------------------------------------------------------*
EG0617
      *---------------------------------------------------------------*
       3900-ATUALIZA-CEDULA        SECTION.
      *---------------------------------------------------------------*

           IF  CCTRO-CUSTO  EQUAL  'MASS'
               GO  TO  3900-99-FIM
           END-IF.

           MOVE RUEC62-CCONTR        TO CCONTR             OF RUECB007.
           EXEC SQL
               SELECT  CJUNC_DEPDC
                    ,  CCDULA
                 INTO :RUECB007.CJUNC-DEPDC
                    , :RUECB007.CCDULA
               FROM    DB2PRD.CEDULA_TEMPR
               WHERE   CCONTR           = :RUECB007.CCONTR
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0270'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE  2                     TO CIMPRE-CDULA-TEMPR

           EXEC SQL
               UPDATE DB2PRD.CEDULA_TEMPR
                  SET CIMPRE_CDULA_TEMPR = :RUECB007.CIMPRE-CDULA-TEMPR
               WHERE  CJUNC_DEPDC        = :RUECB007.CJUNC-DEPDC
                 AND  CCDULA             = :RUECB007.CCDULA
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC62-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CEDULA_TEMPR     ' TO ERR-DBD-TAB
               MOVE 'UPDATE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0280'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3900-99-FIM.                EXIT.
      *---------------------------------------------------------------**

      *---------------------------------------------------------------*
       4000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           IF  RUEC62-COD-RET      EQUAL 99
               MOVE 'RUEC8808'     TO ERR-MODULO
               MOVE ERRO-AREA      TO RUEC62-ERRO-AREA
               MOVE SQLCA(1:136)   TO RUEC62-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                EXIT.
      *---------------------------------------------------------------**

BR1119*---------------------------------------------------------------*
BR1119 9999-CARREGAR-FUMAGEIRA     SECTION.
BR1119*---------------------------------------------------------------*
BR1119
BR1119     MOVE CCONTR OF RUFIB002 TO CCONTR             OF RUECB014
BR1119     MOVE CCONTR OF RUFIB002 TO CCONTR-CREDT-RURAL OF RUECB09E
BR1119     MOVE NENVIO-RECOR       OF RUECB099
BR1119                             TO NENVIO-RECOR       OF RUECB09E
BR1119     MOVE 'P'                TO CTPO-RESP-PSSOA    OF RUECB09E
BR1119     MOVE CCONTR OF RUECB040 TO CCONTR             OF RUECB09E
BR1119     MOVE CSEQ-MATR-RURAL    OF RUECB040
BR1119                             TO CSEQ-MATR-RURAL    OF RUECB09E
BR1119     EXEC SQL
BR1119       SELECT  CTPO_PSSOA,
BR1119               CCGC_CPF,
BR1119               CFLIAL_CGC,
BR1119               CCTRL_CPF_CGC
BR1119       INTO    :RUECB014.CTPO-PSSOA
BR1119             , :RUECB014.CCGC-CPF
BR1119             , :RUECB014.CFLIAL-CGC
BR1119             , :RUECB014.CCTRL-CPF-CGC
BR1119         FROM  DB2PRD.PESSOA_CONTR_RURAL
BR1119        WHERE  CCONTR = :RUECB014.CCONTR
BR1119          AND  CTPO_RESP_PSSOA = 'T'
BR1119          ORDER BY CSEQ_PSSOA ASC
BR1119          FETCH FIRST 1 ROW ONLY
BR1119     END-EXEC
BR1119
BR1119     IF (SQLCODE                  NOT EQUAL ZEROS AND +100)
BR1119         MOVE 99                  TO RUEC62-COD-RET
BR1119         MOVE 'ERRO ACESSO DB2'   TO RUEC62-MENSAGEM
BR1119         MOVE 'DB2'               TO ERR-TIPO-ACESSO
BR1119         MOVE 'PESSOA_CONTR_RURAL' TO ERR-DBD-TAB
BR1119         MOVE 'SELECT'            TO ERR-FUN-COMANDO
BR1119         MOVE SQLCODE             TO ERR-SQL-CODE
BR1119         MOVE '9999'              TO ERR-LOCAL
BR1119         MOVE SPACES              TO ERR-SEGM
BR1119         PERFORM 4000-ROTINA-FIM
BR1119     END-IF.
BR1119
BR1119     IF SQLCODE EQUAL ZEROS
BR1119        MOVE CTPO-PSSOA      OF RUECB014
BR1119                             TO CTPO-PSSOA         OF RUECB09E
BR1119        MOVE CCGC-CPF  OF RUECB014
BR1119                             TO CCNPJ-CPF          OF RUECB09E
BR1119        MOVE CFLIAL-CGC OF RUECB014
BR1119                             TO CFLIAL-CNPJ        OF RUECB09E
BR1119        MOVE CCTRL-CPF-CGC      OF RUECB014
BR1119                             TO CCTRL-CNPJ-CPF     OF RUECB09E
BR1119     END-IF.
BR1119*---------------------------------------------------------------*
BR1119 9999-99-FIM.                EXIT.
BR1119*---------------------------------------------------------------**
