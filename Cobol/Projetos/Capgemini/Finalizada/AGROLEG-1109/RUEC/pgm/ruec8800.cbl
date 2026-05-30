      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8800.
       AUTHOR.     MARINA FUJITA.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8800                                    *
      *    ANALISTA....:   MARINA FUJITA           - SONDA PROCWORK    *
      *    DATA........:   21/06/2012                                  *
      *    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
      *                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ENQUADRAMENTO RECOR                         *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC58 - AREA DE RETORNO DO MODULO.                       *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *================================================================*
EG1015*===============================================================*
EG1015*                   A L T E R A C A O                           *
EG1015*===============================================================*
EG1015*                                                               *
EG1015*     ANALISTA ......: EVANDRO GUIMARAES    - BSI               *
EG1015*     DATA ..........: OUTUBRO/2015                             *
EG1015*     OBJETIVO ......: INCLUIR OPCAO 'NAO SE APLICA' PARA       *
EG1015*                      CONSORCIO E ZONEAMENTO                   *
EG1015*===============================================================*
EG1015
WIP001*===============================================================*
-     *                   A L T E R A C A O                           *
-     *===============================================================*
-     *                                                               *
-     *     ANALISTA ......: WIPRO                - WIPRO             *
-     *     DATA ..........: OUTUBRO/2022                             *
-     *     OBJETIVO ......: ALTERAR O CAMPO WRK-SIST-PROD-RECOR      *
-     *                      SICOR 5.05                              *
WIP001*===============================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
MJ0124*                ALTERACAO - BRADESCO - DS                       *
MJ0124*----------------------------------------------------------------*
MJ0124*                                                                *
MJ0124* ANALISTA REPONSAVEL: MILTON JANUARIO                           *
MJ0124* DATA...............: 12/01/2024                                *
MJ0124* OBJETIVOS..........: INCLUSAO DO ITEM 05 - SILAGEM NO GRUPO    *
MJ0124*                      GRAO/SEMENTE                              *
STE001*================================================================*
STE001*                ALTERACAO - STEFANINI                           *
STE001*----------------------------------------------------------------*
STE001*                                                                *
STE001* ANALISTA REPONSAVEL: VICTORINO                                 *
STE001* DATA...............: MAIO / 2024                               *
STE001* OBJETIVOS..........: INCLUSAO DE OUTRAS PRATICAS DE MANEJO     *
STE001*                      SICOR 5.08                                *
STE001*================================================================*
CCM01A*================================================================*
CCM01A*                ALTERACAO - CAPGEMINI                           *
CCM01A*----------------------------------------------------------------*
CCM01A*                                                                *
CCM01A* ANALISTA REPONSAVEL: CRISTIANY MACAN                           *
CCM01A* DATA...............: JANEIRO /2026                             *
CCM01A* OBJETIVOS..........: AGROLEG-404 - RESOLUCAO 5247 - RENEG -    *
CCM01A*                      ACEITAR CODIGO DE ZONEAMENTO IGUAL A ZEROS*
CCM01A*================================================================*
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
           '*** INICIO DA WORKING RUEC8800 ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       01  FILLER.
           03  WRK-CONSORC-NUM         PIC 9(001)      VALUE ZEROS.
           03  FILLER  REDEFINES  WRK-CONSORC-NUM.
               05  WRK-CONSORC-CHAR    PIC X(001).

       01  FILLER.
           03  WRK-ZONEAM-NUM          PIC 9(001)      VALUE ZEROS.
           03  FILLER  REDEFINES  WRK-ZONEAM-NUM.
               05  WRK-ZONEAM-CHAR     PIC X(001).

       01  FILLER.
           03  WRK-GRAOSEM-NUM         PIC 9(001)      VALUE ZEROS.
           03  FILLER  REDEFINES  WRK-GRAOSEM-NUM.
               05  WRK-GRAOSEM-CHAR    PIC X(001).

       01  FILLER.
           03  WRK-EMPREEND-RECOR      PIC X(014)      VALUE SPACES.
           03  FILLER  REDEFINES  WRK-EMPREEND-RECOR.
               05  WRK-ATIV-RECOR      PIC X(001).
               05  WRK-FINAL-RECOR     PIC X(001).
               05  WRK-MODALID-RECOR   PIC X(002).
               05  WRK-PRODUTO-RECOR   PIC X(004).
               05  WRK-VARIED-RECOR    PIC X(003).
               05  WRK-CONSORC-RECOR   PIC X(001).
               05  WRK-CESTA-RECOR     PIC X(001).
               05  WRK-ZONEAM-RECOR    PIC X(001).

WIP001     03  WRK-SIST-PROD-RECOR     PIC X(013)      VALUE SPACES.
           03  FILLER  REDEFINES  WRK-SIST-PROD-RECOR.
               05  WRK-TPAGROP-RECOR   PIC X(001).
               05  WRK-TPINTEG-RECOR   PIC X(001).
               05  WRK-GRAOSEM-RECOR   PIC X(001).
               05  WRK-TPIRRIG-RECOR   PIC X(002).
               05  WRK-TPCULTV-RECOR   PIC X(002).
               05  WRK-FASECIC-RECOR   PIC X(002).
PRI499         05  WRK-TPCLIMA-RECOR   PIC 9(002).
STE001         05  WRK-OUTPRATMANEJO   PIC 9(002).

           03  WRK-DATA-DB2            PIC X(010)     VALUE SPACES.
           03  FILLER  REDEFINES  WRK-DATA-DB2.
               05  WRK-DIA-DB2         PIC 9(002).
               05  FILLER              PIC X(001).
               05  WRK-MES-DB2         PIC 9(002).
               05  FILLER              PIC X(001).
               05  WRK-ANO-DB2         PIC 9(004).

           03  WRK-DATA-AUX-1          PIC 9(008)     VALUE ZEROS.

           03  WRK-DATA-AUX-2          PIC 9(008)     VALUE ZEROS.

PRI499 01  WRK-CRUR2G0C                PIC X(008)     VALUE 'CRUR2G0C'.
STE001 01  WRK-CRUR2G3C                PIC X(008)     VALUE 'CRUR2G3C'.
STEFAB 01  WRK-CRUR240G                PIC X(008)     VALUE 'CRUR240G'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA FORMATACAO DE ERRO ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

PRI499*----------------------------------------------------------------*
PRI499 01  FILLER                      PIC  X(050)         VALUE
PRI499     '*** AREA DE ERRO DA CRUR2G0C ***'.
PRI499*----------------------------------------------------------------*
PRI499
PRI499 01  WRK-ERRO-CRUR2G0C.
PRI499     05  FILLER                  PIC  X(045)         VALUE
PRI499         '* RETURN-CODE DIFERENTE DE ZEROS NO CRUR2G0C '.
PRI499     05  FILLER                  PIC  X(012)         VALUE
PRI499         '- RET.COD = '.
PRI499     05  WRK-RETURN-CRUR2G0C     PIC  9(002)         VALUE ZEROS.
PRI499     05  FILLER                  PIC  X(011)         VALUE
PRI499         ' - LOCAL = '.
PRI499     05  WRK-LOCAL-CRUR2G0C      PIC  9(002)         VALUE ZEROS.
PRI499     05  FILLER                  PIC  X(002)         VALUE ' *'.

STE001*----------------------------------------------------------------*
STE001 01  FILLER                      PIC  X(050)         VALUE
STE001     '*** AREA DE ERRO DA CRUR2G3C ***'.
STE001*----------------------------------------------------------------*
STE001
STE001 01  WRK-ERRO-CRUR2G3C.
STE001     05  FILLER                  PIC  X(045)         VALUE
STE001         '* RETURN-CODE DIFERENTE DE ZEROS NO CRUR2G3C '.
STE001     05  FILLER                  PIC  X(012)         VALUE
STE001         '- RET.COD = '.
STE001     05  WRK-RETURN-CRUR2G3C     PIC  9(002)         VALUE ZEROS.
STE001     05  FILLER                  PIC  X(011)         VALUE
STE001         ' - LOCAL = '.
STE001     05  WRK-LOCAL-CRUR2G3C      PIC  9(002)         VALUE ZEROS.
STE001     05  FILLER                  PIC  X(002)         VALUE ' *'.

STEFAB*----------------------------------------------------------------*
STEFAB 01  FILLER                      PIC  X(050)         VALUE
STEFAB     '*** AREA DE ERRO DA CRUR240G ***'.
STEFAB*----------------------------------------------------------------*
STEFAB
STEFAB 01  WRK-ERRO-CRUR240G.
STEFAB     05  FILLER                  PIC  X(045)         VALUE
STEFAB         '* RETURN-CODE DIFERENTE DE ZEROS NO CRUR240G '.
STEFAB     05  FILLER                  PIC  X(012)         VALUE
STEFAB         '- RET.COD = '.
STEFAB     05  WRK-RETURN-CRUR240G     PIC  9(002)         VALUE ZEROS.
STEFAB     05  FILLER                  PIC  X(011)         VALUE
STEFAB         ' - LOCAL = '.
STEFAB     05  WRK-LOCAL-CRUR240G      PIC  9(002)         VALUE ZEROS.
STEFAB     05  FILLER                  PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TRATAR CAMPOS NULOS ***'.
      *---------------------------------------------------------------*

       01  WRK-NULL-B086.
           05 WRK-B086-DINIC-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B086-DFIM-NULL        PIC S9(04) COMP VALUE +0.

PRI499*----------------------------------------------------------------*
PRI499 01  FILLER                      PIC  X(032)         VALUE
PRI499     '*** AREA DO MODULO CRUR2G0C  ***'.
PRI499*----------------------------------------------------------------*
PRI499
PRI499 01  WRK-AREA-CRUR2G0C.
PRI499     COPY CRURW00W.
PRI499     COPY CRURWG0I.

STE001*----------------------------------------------------------------*
STE001 01  FILLER                      PIC  X(032)         VALUE
STE001     '*** AREA DO MODULO CRUR2G3C  ***'.
STE001*----------------------------------------------------------------*
STE001
STE001 01  WRK-AREA-CRUR2G3C.
STE001     COPY CRURW00W.
STE001     COPY CRURWG3Y.

STEFAB*----------------------------------------------------------------*
STEFAB 01  FILLER                      PIC  X(032)         VALUE
STEFAB     '*** AREA DO MODULO CRUR240G  ***'.
STEFAB*----------------------------------------------------------------*
STEFAB
STEFAB 01  WRK-AREA-CRUR240G.
STEFAB     COPY CRURW00W.
STEFAB     COPY CRURWPGI.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.
           EXEC SQL INCLUDE RURCV002 END-EXEC.
           EXEC SQL INCLUDE RURCB003 END-EXEC.
           EXEC SQL INCLUDE RURCB004 END-EXEC.
           EXEC SQL INCLUDE RURCB006 END-EXEC.
           EXEC SQL INCLUDE RURCB082 END-EXEC.
           EXEC SQL INCLUDE RURCB083 END-EXEC.
           EXEC SQL INCLUDE RURCB084 END-EXEC.
           EXEC SQL INCLUDE RURCB085 END-EXEC.
           EXEC SQL INCLUDE RURCB086 END-EXEC.
           EXEC SQL INCLUDE RURCB087 END-EXEC.
           EXEC SQL INCLUDE RURCB088 END-EXEC.
           EXEC SQL INCLUDE RURCB089 END-EXEC.
           EXEC SQL INCLUDE RURCB090 END-EXEC.
           EXEC SQL INCLUDE RURCB091 END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8800 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC58'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC58-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL  'CKRS1000'.

           INITIALIZE RUEC58-RETORNO.

           MOVE ZEROS              TO RUEC58-COD-RET.
           MOVE 'CONSULTA EFETUADA COM SUCESSO'
                                   TO RUEC58-MENSAGEM.

           PERFORM  1000-PROCESSAMENTO-PRINCIPAL.
           PERFORM  4000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAMENTO-PRINCIPAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

           IF  RUEC58-COD-RET  EQUAL  ZEROS
               PERFORM 3000-OBTER-EMPREEND
               PERFORM 3100-OBTER-SIST-PROD
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC58-PGM-CHAMADOR EQUAL SPACES
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'PROGRAMA CHAMADOR NAO PREENCHIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-AGENCIA      NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO AGENCIA INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-CEDULA       NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'NUMERO CEDULA INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-CONTRATO     NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'NUMERO CONTRATO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-FINALIDADE   NOT  NUMERIC)
           OR (RUEC58-FINALIDADE   EQUAL ZEROS)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO FINALIDADE INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-ATIVIDADE    NOT  NUMERIC)
           OR (RUEC58-ATIVIDADE    EQUAL ZEROS)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO ATIVIDADE INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-MODALIDADE   NOT  NUMERIC)
           OR (RUEC58-MODALIDADE   EQUAL ZEROS)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO MODALIDADE INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-PRODUTO      NOT  NUMERIC)
           OR (RUEC58-PRODUTO      EQUAL ZEROS)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO PRODUTO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-VARIEDADE    NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO VARIEDADE INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-CESTA-SAFRA  NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO CESTA SAFRA INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-ZONEAMENTO   NOT  NUMERIC)
CCM01A********(RUEC58-ZONEAMENTO   EQUAL ZEROS)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO ZONEAMENTO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-CONSORCIO    NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CODIGO CONSORCIO  INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-TIPO-AGROP   NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'TIPO AGROPECUARIO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-TIPO-INTEG   NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'TIPO INTEGRACAO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-GRAO-SEMENTE NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'GRAO OU SEMENTE INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-TIPO-IRRIG   NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'TIPO IRRIGACAO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-TIPO-CULTIVO NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'TIPO CULTIVO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-CICLO-PROD   NOT  NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'CICLO PRODUCAO INVALIDO'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

PRI499     IF (RUEC58-CLIMA-PROD   NOT  NUMERIC)
PRI499         MOVE 01             TO RUEC58-COD-RET
PRI499         MOVE 'TIPO DE CLIMA INVALIDO'
PRI499                             TO RUEC58-MENSAGEM
PRI499         GO  TO  2000-99-FIM
PRI499     END-IF.

STE001     IF (RUEC58-COUTRO-TRATO-SOLO  NOT  NUMERIC)
STE001         MOVE 01             TO RUEC58-COD-RET
STE001         MOVE 'PRATICA DE MANEJO INVALIDO'
STE001                             TO RUEC58-MENSAGEM
STE001         GO  TO  2000-99-FIM
STE001     END-IF.

           IF (RUEC58-DCDULA       EQUAL SPACES)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'DATA DA CEDULA NAO INFORMADA'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-DCDULA(1:2)  NOT NUMERIC)
           OR (RUEC58-DCDULA(4:2)  NOT NUMERIC)
           OR (RUEC58-DCDULA(7:4)  NOT NUMERIC)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'DATA DA CEDULA INVALIDA'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC58-DCDULA(1:2)  EQUAL ZEROS)
           OR (RUEC58-DCDULA(4:2)  EQUAL ZEROS)
           OR (RUEC58-DCDULA(7:4)  EQUAL ZEROS)
               MOVE 01             TO RUEC58-COD-RET
               MOVE 'DATA DA CEDULA INVALIDA'
                                   TO RUEC58-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OBTER-EMPREEND         SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC58-FINALIDADE  TO CFNALD-RURAL       OF RURCB084
           MOVE RUEC58-ATIVIDADE   TO CATVDD-RURAL       OF RURCB084
           MOVE RUEC58-MODALIDADE  TO CMODLD-RURAL       OF RURCB084
           MOVE RUEC58-PRODUTO     TO CPRODT-RURAL       OF RURCB084
           MOVE RUEC58-VARIEDADE   TO CVAR-PRODT-RURAL   OF RURCB084
           MOVE RUEC58-CESTA-SAFRA TO CCESTA-SAFRA-RURAL OF RURCB084
           MOVE RUEC58-ZONEAMENTO  TO CINDCD-ZNMTO       OF RURCB084
           MOVE RUEC58-CONSORCIO   TO CINDCD-CSCIO       OF RURCB084

           EXEC SQL
               SELECT   NEMPTO_RURAL,
                        CUND_MEDD_FINCD,
                        CUND_MEDD_PROVV,
                        CCTRO_CUSTO,
                        DINIC_VGCIA,
                        DFIM_VGCIA
               INTO :RURCB084.NEMPTO-RURAL,
                    :RURCB084.CUND-MEDD-FINCD,
                    :RURCB084.CUND-MEDD-PROVV,
                    :RURCB084.CCTRO-CUSTO,
                    :RURCB084.DINIC-VGCIA,
                    :RURCB084.DFIM-VGCIA
                 FROM DB2PRD.TEMPTO_FINAN_RURAL
                WHERE CFNALD_RURAL        = :RURCB084.CFNALD-RURAL
                  AND CATVDD_RURAL        = :RURCB084.CATVDD-RURAL
                  AND CMODLD_RURAL        = :RURCB084.CMODLD-RURAL
                  AND CPRODT_RURAL        = :RURCB084.CPRODT-RURAL
                  AND CVAR_PRODT_RURAL    = :RURCB084.CVAR-PRODT-RURAL
                  AND CCESTA_SAFRA_RURAL  = :RURCB084.CCESTA-SAFRA-RURAL
                  AND CINDCD_ZNMTO        = :RURCB084.CINDCD-ZNMTO
                  AND CINDCD_CSCIO        = :RURCB084.CINDCD-CSCIO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TEMPTO_FINAN_RURAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'EMPREENDIMENTO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CUND-MEDD-FINCD OF RURCB084  TO CUND-MEDD OF RURCV002

           EXEC SQL
               SELECT   IUND_MEDD
                      , CSGL_UND_MEDD
               INTO :RURCV002.IUND-MEDD
                  , :RURCV002.CSGL-UND-MEDD
                 FROM DB2PRD.V01UNIDADE_MEDD
                WHERE CUND_MEDD           = :RURCV002.CUND-MEDD
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'UNIDADE_MEDD     ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'UNIDADE DE MEDIDA  CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CUND-MEDD     OF RURCV002  TO RUEC58-CUND-MEDD-FINCD
           MOVE IUND-MEDD     OF RURCV002  TO RUEC58-IUND-MEDD-FINCD
           MOVE CSGL-UND-MEDD OF RURCV002  TO RUEC58-SIGL-MEDD-FINCD.

           MOVE CUND-MEDD-PROVV OF RURCB084  TO CUND-MEDD OF RURCV002

           EXEC SQL
               SELECT   IUND_MEDD
                      , CSGL_UND_MEDD
               INTO :RURCV002.IUND-MEDD
                  , :RURCV002.CSGL-UND-MEDD
                 FROM DB2PRD.V01UNIDADE_MEDD
                WHERE CUND_MEDD           = :RURCV002.CUND-MEDD
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'UNIDADE_MEDD     ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0030'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'UNIDADE DE MEDIDA  CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CUND-MEDD     OF RURCV002  TO RUEC58-CUND-MEDD-PROVV
           MOVE IUND-MEDD     OF RURCV002  TO RUEC58-IUND-MEDD-PROVV
           MOVE CSGL-UND-MEDD OF RURCV002  TO RUEC58-SIGL-MEDD-PROVV.

           MOVE DINIC-VGCIA   OF RURCB084  TO RUEC58-DINIC-VGCIA
           MOVE DFIM-VGCIA    OF RURCB084  TO RUEC58-DFIM-VGCIA.

           IF  (CCTRO-CUSTO OF RURCB084 NOT  EQUAL  SPACES)
           AND (RUEC58-PGM-CHAMADOR(1:4) NOT EQUAL CCTRO-CUSTO(1:4))
               MOVE 05                  TO RUEC58-COD-RET
               MOVE 'CENTRO DE CUSTO NAO PERMITIDO'
                                        TO RUEC58-MENSAGEM
           END-IF.

           MOVE RUEC58-DCDULA              TO WRK-DATA-DB2.
           STRING WRK-ANO-DB2  WRK-MES-DB2  WRK-DIA-DB2
                   DELIMITED BY ' '      INTO WRK-DATA-AUX-1.

           MOVE DINIC-VGCIA   OF RURCB084  TO WRK-DATA-DB2.
           STRING WRK-ANO-DB2  WRK-MES-DB2  WRK-DIA-DB2
                   DELIMITED BY ' '      INTO WRK-DATA-AUX-2.

           IF  WRK-DATA-AUX-1  LESS  WRK-DATA-AUX-2
               MOVE 04                  TO RUEC58-COD-RET
               MOVE 'CEDULA ANTERIOR AO INICIO DA VIGENCIA EMPREEND'
                                        TO RUEC58-MENSAGEM
           END-IF.

           MOVE DFIM-VGCIA    OF RURCB084  TO WRK-DATA-DB2.
           STRING WRK-ANO-DB2  WRK-MES-DB2  WRK-DIA-DB2
                   DELIMITED BY ' '      INTO WRK-DATA-AUX-2.

           IF  WRK-DATA-AUX-1  GREATER  WRK-DATA-AUX-2
               MOVE 04                  TO RUEC58-COD-RET
               MOVE 'CEDULA POSTEIOR AO TERMINO DA VIGENCIA EMPREEND'
                                        TO RUEC58-MENSAGEM
           END-IF.

           MOVE RUEC58-FINALIDADE       TO CFNALD-RURAL  OF RURCB003.

           EXEC SQL
               SELECT   CFNALD_RECOR
               INTO :RURCB003.CFNALD-RECOR
                 FROM DB2PRD.FNALD_RURAL
                WHERE CFNALD_RURAL        = :RURCB003.CFNALD-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'FNALD_RURAL      ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0040'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'FINALIDADE NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CFNALD-RECOR(5:1)       TO WRK-FINAL-RECOR.

           MOVE RUEC58-ATIVIDADE        TO CATVDD-RURAL  OF RURCB004.

           EXEC SQL
               SELECT   CATVDD_BACEN
               INTO :RURCB004.CATVDD-BACEN
                 FROM DB2PRD.ATIVIDADE_RURAL
                WHERE CATVDD_RURAL        = :RURCB004.CATVDD-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'ATIVIDADE_RURAL  ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0050'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'ATIVIDADE  NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CATVDD-BACEN(5:1)       TO WRK-ATIV-RECOR.

           MOVE RUEC58-MODALIDADE       TO CMODLD-RURAL  OF RURCB085.

           EXEC SQL
               SELECT   CMODLD_RECOR
               INTO :RURCB085.CMODLD-RECOR
                 FROM DB2PRD.TMODLD_EMPTO_RURAL
                WHERE CMODLD_RURAL        = :RURCB085.CMODLD-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TMODLD_EMPTO_RURAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0060'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'MODALIDADE NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CMODLD-RECOR(4:2)       TO WRK-MODALID-RECOR.

           MOVE RUEC58-PRODUTO          TO CPRODT-RURAL  OF RURCB006.

           EXEC SQL
               SELECT   CPRODT_BACEN
               INTO :RURCB006.CPRODT-BACEN
                 FROM DB2PRD.PRODUTO_RURAL
                WHERE CPRODT_RURAL        = :RURCB006.CPRODT-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PRODUTO_RURAL    ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0070'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'PRODUTO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CPRODT-BACEN(2:4)       TO WRK-PRODUTO-RECOR.

           MOVE RUEC58-VARIEDADE        TO CVAR-PRODT-RURAL OF RURCB091.

           EXEC SQL
               SELECT   CVAR_PRODT_RECOR
               INTO :RURCB091.CVAR-PRODT-RECOR
                 FROM DB2PRD.TVAR_PRODT_RURAL
                WHERE CVAR_PRODT_RURAL    = :RURCB091.CVAR-PRODT-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TVAR_PRODT_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0080'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'VARIEDADE NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CVAR-PRODT-RECOR(3:3)       TO WRK-VARIED-RECOR.

EG1015     IF  RUEC58-CONSORCIO    NOT EQUAL 0 AND 8 AND 4
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'CODIGO CONSORCIO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE RUEC58-CONSORCIO            TO WRK-CONSORC-NUM.
           MOVE WRK-CONSORC-CHAR            TO WRK-CONSORC-RECOR.

EG1015     IF  RUEC58-ZONEAMENTO   NOT EQUAL 1 AND 2 AND 8
CCM01A                                         AND 0
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'CODIGO ZONEAMENTO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE RUEC58-ZONEAMENTO           TO WRK-ZONEAM-NUM.
           MOVE WRK-ZONEAM-CHAR             TO WRK-ZONEAM-RECOR.

           MOVE RUEC58-CESTA-SAFRA    TO CCESTA-SAFRA-RURAL OF RURCB082.

           EXEC SQL
               SELECT   CCESTA_SAFRA_RECOR
               INTO :RURCB082.CCESTA-SAFRA-RECOR
                 FROM DB2PRD.TCESTA_SAFRA_RURAL
                WHERE CCESTA_SAFRA_RURAL  = :RURCB082.CCESTA-SAFRA-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TCESTA_SAFRA_RURAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0090'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 02                  TO RUEC58-COD-RET
               MOVE 'CESTA SAFRA NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CCESTA-SAFRA-RECOR(5:1)     TO WRK-CESTA-RECOR.
           MOVE WRK-EMPREEND-RECOR          TO RUEC58-EMPREEND.

      *---------------------------------------------------------------*
       3000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-OBTER-SIST-PROD        SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                   TO WRK-SIST-PROD-RECOR.

           MOVE RUEC58-TIPO-AGROP       TO CTPO-AGROP-RURAL OF RURCB087.

           EXEC SQL
               SELECT   CTPO_AGROP_RECOR
               INTO :RURCB087.CTPO-AGROP-RECOR
                 FROM DB2PRD.TTPO_AGROP_RURAL
                WHERE CTPO_AGROP_RURAL   = :RURCB087.CTPO-AGROP-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TTPO_AGROP_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0100'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'TIPO AGROPECUARIA NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CTPO-AGROP-RECOR(5:1)   TO WRK-TPAGROP-RECOR.

           MOVE RUEC58-TIPO-INTEG       TO CTPO-INTGC-RURAL OF RURCB090.

           EXEC SQL
               SELECT   CTPO_INTGC_RECOR
               INTO :RURCB090.CTPO-INTGC-RECOR
                 FROM DB2PRD.TTPO_INTGC_RURAL
                WHERE CTPO_INTGC_RURAL   = :RURCB090.CTPO-INTGC-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TTPO_INTGC_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0110'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'TIPO INTEGRACAO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CTPO-INTGC-RECOR(5:1)   TO WRK-TPINTEG-RECOR.

MJ0124     IF  RUEC58-GRAO-SEMENTE NOT EQUAL 0 AND 3 AND 4 AND 8 AND 5
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'GRAO/SEMENTE NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE RUEC58-GRAO-SEMENTE     TO WRK-GRAOSEM-NUM.
           MOVE WRK-GRAOSEM-CHAR        TO WRK-GRAOSEM-RECOR.

           MOVE RUEC58-TIPO-IRRIG       TO CTPO-IGCAO-RURAL OF RURCB089.

           EXEC SQL
               SELECT   CTPO_IGCAO_RECOR
               INTO :RURCB089.CTPO-IGCAO-RECOR
                 FROM DB2PRD.TTPO_IGCAO_RURAL
                WHERE CTPO_IGCAO_RURAL   = :RURCB089.CTPO-IGCAO-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TTPO_IGCAO_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0120'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'TIPO IRRIGACAO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CTPO-IGCAO-RECOR(4:2)   TO WRK-TPIRRIG-RECOR.

           MOVE RUEC58-TIPO-CULTIVO     TO CTPO-CTIVO-RURAL OF RURCB088.

           EXEC SQL
               SELECT   CTPO_CTIVO_RECOR
               INTO :RURCB088.CTPO-CTIVO-RECOR
                 FROM DB2PRD.TTPO_CTIVO_RURAL
                WHERE CTPO_CTIVO_RURAL   = :RURCB088.CTPO-CTIVO-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TTPO_CTIVO_RURAL ' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0130'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'TIPO CULTIVO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CTPO-CTIVO-RECOR(4:2)   TO WRK-TPCULTV-RECOR.

           MOVE RUEC58-CICLO-PROD     TO CCICLO-PROD-RURAL OF RURCB083.

           EXEC SQL
               SELECT   CCICLO_PROD_RECOR
               INTO :RURCB083.CCICLO-PROD-RECOR
                 FROM DB2PRD.TCICLO_PROD_RURAL
                WHERE CCICLO_PROD_RURAL   = :RURCB083.CCICLO-PROD-RURAL
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TCICLO_PROD_RURAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0140'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'CICLO PRODUCAO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           MOVE CCICLO-PROD-RECOR(4:2)  TO WRK-FASECIC-RECOR.

PRI499     PERFORM 3102-00-SELECT-CRURB0WY.
PRI499
PRI499     IF  CRURW00W-COD-RETORNO     OF WRK-AREA-CRUR2G0C
                                        EQUAL   10
PRI499         MOVE 03                  TO RUEC58-COD-RET
PRI499         MOVE 'TIPO DE CLIMA CADASTRADO'
PRI499                                  TO RUEC58-MENSAGEM
PRI499         PERFORM 4000-ROTINA-FIM
PRI499     END-IF.
PRI499
PRI499     MOVE CRURWG0I-S-CCLIMT-SIST  TO WRK-TPCLIMA-RECOR.
PRI499
PRI499     MOVE WRK-SIST-PROD-RECOR     TO RUEC58-SIST-PROD.
PRI499
STE001     PERFORM 3103-00-SELECT-MANEJO.
STE001
STE001     IF  CRURW00W-COD-RETORNO     OF WRK-AREA-CRUR2G3C
STE001                                  EQUAL   10
STE001         MOVE 03                  TO RUEC58-COD-RET
STE001         MOVE 'PRATICA DE MANEJO NAO CADASTRADO'
STE001                                  TO RUEC58-MENSAGEM
STE001         PERFORM 4000-ROTINA-FIM
STE001     END-IF.
STE001
STE001     IF CRURWG3Y-S-COUTRO-SOLO    NOT NUMERIC
STE001        MOVE 00                   TO WRK-OUTPRATMANEJO
STE001     ELSE
STE001        MOVE CRURWG3Y-S-COUTRO-SOLO
STE001                                  TO WRK-OUTPRATMANEJO
STE001     END-IF.
STE001
STE001     MOVE WRK-SIST-PROD-RECOR     TO RUEC58-SIST-PROD.
STE001
STEFAB     PERFORM 3104-00-SELECT-OUTROS.
STEFAB*
STEFAB*    _F  CRURW00W-COD-RETORNO     OF WRK-AREA-CRUR240G
STEFAB*                                 EQUAL   10
STEFAB*        _OVE 04                  TO RUEC58-COD-RET
STEFAB*        _OVE 'TIPO OUTROS CONF. NAO CADASTRADO'
STEFAB*                                 TO RUEC58-MENSAGEM
STEFAB*        _ERFORM 4000-ROTINA-FIM
STEFAB*    _ND-IF.
STEFAB
STEFAB     IF CRURWPGI-S-COUTRO-CONFM-BACEN(1)
STEFAB                                  NOT NUMERIC
STEFAB        MOVE ZEROS                TO RUEC58-TP-CONFDD-OTR-N2
STEFAB     ELSE
STEFAB        IF  CRURWPGI-S-COUTRO-TPO-CONFM(1)
STEFAB                                  EQUAL
STEFAB                                  CRURWPGI-E-COUTRO-TPO-CONFM
STEFAB            MOVE CRURWPGI-S-COUTRO-CONFM-BACEN(1)
STEFAB                                  TO RUEC58-TP-CONFDD-OTR-N2
STEFAB        ELSE
STEFAB            MOVE ZEROS            TO RUEC58-TP-CONFDD-OTR-N2
STEFAB        END-IF
STEFAB     END-IF.
STEFAB
           MOVE NEMPTO-RURAL OF RURCB084
                                        TO NEMPTO-RURAL     OF RURCB086.
           MOVE RUEC58-TIPO-AGROP       TO CTPO-AGROP-RURAL OF RURCB086.
           MOVE RUEC58-TIPO-INTEG       TO CTPO-INTGC-RURAL OF RURCB086.
           MOVE RUEC58-GRAO-SEMENTE     TO CINDCD-UND-PRDTV OF RURCB086.
           MOVE RUEC58-TIPO-IRRIG       TO CTPO-IGCAO-RURAL OF RURCB086.
           MOVE RUEC58-TIPO-CULTIVO     TO CTPO-CTIVO-RURAL OF RURCB086.
           MOVE RUEC58-CICLO-PROD       TO
                                          CCICLO-PROD-RURAL OF RURCB086.
PRI499     MOVE RUEC58-CLIMA-PROD       TO
PRI499                                   CCLIMT-RURAL-BACEN OF RURCB086.
STE001     MOVE RUEC58-COUTRO-TRATO-SOLO   TO
STE001                                   COUTRO-TRATO-SOLO  OF RURCB086.
           EXEC SQL
               SELECT   DINIC_VGCIA,
                        DFIM_VGCIA
               INTO :RURCB086.DINIC-VGCIA
                         :WRK-B086-DINIC-NULL,
                    :RURCB086.DFIM-VGCIA
                         :WRK-B086-DFIM-NULL
                 FROM DB2PRD.TSIST_PROD_RURAL
                WHERE NEMPTO_RURAL        = :RURCB086.NEMPTO-RURAL
                 AND (CTPO_AGROP_RURAL    = :RURCB086.CTPO-AGROP-RURAL
                   OR CTPO_AGROP_RURAL    = 99999)
                 AND (CTPO_INTGC_RURAL    = :RURCB086.CTPO-INTGC-RURAL
                   OR CTPO_INTGC_RURAL    = 99999)
                 AND (CINDCD_UND_PRDTV    = :RURCB086.CINDCD-UND-PRDTV
                   OR CINDCD_UND_PRDTV    = 9)
                 AND (CTPO_IGCAO_RURAL    = :RURCB086.CTPO-IGCAO-RURAL
                   OR CTPO_IGCAO_RURAL    = 99999)
                 AND (CTPO_CTIVO_RURAL    = :RURCB086.CTPO-CTIVO-RURAL
                   OR CTPO_CTIVO_RURAL    = 99999)
                 AND (CCICLO_PROD_RURAL   = :RURCB086.CCICLO-PROD-RURAL
                   OR CCICLO_PROD_RURAL   = 99999)
STE001           AND (COUTRO_TRATO_SOLO   = :RURCB086.COUTRO-TRATO-SOLO
STE001             OR COUTRO_TRATO_SOLO   = 99999)
                 FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE        NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC58-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC58-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TSIST_PROD_RURAL'  TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0150'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC58-COD-RET
               MOVE 'SISTEMA PRODUCAO NAO CADASTRADO'
                                        TO RUEC58-MENSAGEM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL ZEROS
               PERFORM 3101-VER-NULOS-B086
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3101-VER-NULOS-B086         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-B086-DINIC-NULL          NOT EQUAL ZEROS
               MOVE SPACES     TO DINIC-VGCIA     OF RURCB086
           END-IF.

           IF  WRK-B086-DFIM-NULL           NOT EQUAL ZEROS
               MOVE SPACES     TO DFIM-VGCIA      OF RURCB086
           END-IF.

      *---------------------------------------------------------------*
       3101-99-FIM.                EXIT.
      *---------------------------------------------------------------*

PRI499*---------------------------------------------------------------*
PRI499 3102-00-SELECT-CRURB0WY         SECTION.
PRI499*---------------------------------------------------------------*
PRI499     INITIALIZE WRK-AREA-CRUR2G0C.
PRI499
PRI499     MOVE 'CRURW00W'             TO  CRURW00W-COD-LAYOUT
STE001                                 OF  WRK-AREA-CRUR2G0C.
PRI499     MOVE 27                     TO  CRURW00W-TAM-LAYOUT
STE001                                 OF  WRK-AREA-CRUR2G0C.
PRI499     MOVE 'CRURWG0I'             TO  CRURWG0I-COD-LAYOUT
PRI499     MOVE 273                    TO  CRURWG0I-TAM-LAYOUT.
PRI499     MOVE RUEC58-CLIMA-PROD      TO  CRURWG0I-E-CCLIMT-RURAL.
PRI499
PRI499     CALL  WRK-CRUR2G0C          USING WRK-AREA-CRUR2G0C.
PRI499
PRI499     IF  CRURW00W-COD-RETORNO    OF  WRK-AREA-CRUR2G0C
STE001                                 NOT EQUAL   00 AND 10
PRI499         MOVE 99                 TO RUEC58-COD-RET
PRI499         MOVE 'ERRO ACESSO DB2'  TO RUEC58-MENSAGEM
PRI499         MOVE 'APL'              TO  ERR-TIPO-ACESSO
PRI499         MOVE  CRURW00W-COD-RETORNO
STE001                                 OF  WRK-AREA-CRUR2G0C
PRI499                                 TO  WRK-RETURN-CRUR2G0C
PRI499         MOVE  11                TO  WRK-LOCAL-CRUR2G0C
PRI499         MOVE  WRK-ERRO-CRUR2G0C TO  ERR-TEXTO
PRI499         PERFORM 4000-ROTINA-FIM
PRI499     END-IF.
PRI499
PRI499*---------------------------------------------------------------*
PRI499 3102-99-FIM.                    EXIT.
PRI499*---------------------------------------------------------------*

STE001*---------------------------------------------------------------*
STE001 3103-00-SELECT-MANEJO           SECTION.
STE001*---------------------------------------------------------------*
STE001     INITIALIZE WRK-AREA-CRUR2G3C.
STE001
STE001     MOVE RUEC58-COUTRO-TRATO-SOLO
STE001                                 TO  CRURWG3Y-E-COUTRO-SOLO.
STE001
STE001     CALL  WRK-CRUR2G3C          USING WRK-AREA-CRUR2G3C.
STE001
STE001     IF  CRURW00W-COD-RETORNO    OF  WRK-AREA-CRUR2G3C
STE001                                 NOT EQUAL   00 AND 10
STE001         MOVE 99                 TO  RUEC58-COD-RET
STE001         MOVE 'ERRO ACESSO DB2'  TO  RUEC58-MENSAGEM
STE001         MOVE 'APL'              TO  ERR-TIPO-ACESSO
STE001         MOVE  CRURW00W-COD-RETORNO
STE001                                 OF  WRK-AREA-CRUR2G3C
STE001                                 TO  WRK-RETURN-CRUR2G3C
STE001         MOVE  11                TO  WRK-LOCAL-CRUR2G3C
STE001         MOVE  WRK-ERRO-CRUR2G3C TO  ERR-TEXTO
STE001         PERFORM 4000-ROTINA-FIM
STE001     END-IF.
STE001
STE001*---------------------------------------------------------------*
STE001 3103-99-FIM.                    EXIT.
STE001*---------------------------------------------------------------*

STEFAB*---------------------------------------------------------------*
STEFAB 3104-00-SELECT-OUTROS           SECTION.
STEFAB*---------------------------------------------------------------*
STEFAB
STEFAB     MOVE SPACES                 TO  CRURWPGI-E-ROUTRO-CONFM-BACEN
STEFAB                                     CRURWPGI-E-CSIT-REG-RURAL
STEFAB     MOVE 1                      TO  CRURWPGI-E-MAX-OCORR
STEFAB     MOVE 'I'                    TO  CRURWPGI-INDICADOR-PAGINACAO
STEFAB     MOVE RUEC58-TP-CONFDD-OTR   TO  CRURWPGI-E-COUTRO-TPO-CONFM
STEFAB
STEFAB     CALL  WRK-CRUR240G          USING WRK-AREA-CRUR240G.
STEFAB
STEFAB*   XIF  CRURW00W-COD-RETORNO    OF  WRK-AREA-CRUR240G
STEFAB*                               XNOT EQUAL 00 AND 10 AND 01
STEFAB*       XMOVE 99                 TO  RUEC58-COD-RET
STEFAB*       XMOVE 'ERRO ACESSO DB2'  TO  RUEC58-MENSAGEM
STEFAB*       XMOVE 'APL'              TO  ERR-TIPO-ACESSO
STEFAB*       XMOVE  CRURW00W-COD-RETORNO
STEFAB*                               XOF  WRK-AREA-CRUR240G
STEFAB*                               XTO  WRK-RETURN-CRUR240G
STEFAB*       XMOVE  12                TO  WRK-LOCAL-CRUR240G
STEFAB*       XMOVE  WRK-ERRO-CRUR240G TO  ERR-TEXTO
STEFAB*       XPERFORM 4000-ROTINA-FIM
STEFAB*   XEND-IF.
STEFAB
STEFAB*---------------------------------------------------------------*
STEFAB 3104-99-FIM.                    EXIT.
STEFAB*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           IF  RUEC58-COD-RET      EQUAL 99
               MOVE 'RUEC8800'     TO ERR-MODULO
               MOVE ERRO-AREA      TO RUEC58-ERRO-AREA
               MOVE SQLCA(1:136)   TO RUEC58-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                EXIT.
      *---------------------------------------------------------------**
