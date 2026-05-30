      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8850.
       AUTHOR.     EDUARDO WATANABE.
      *================================================================*
      *                    B R Q                                       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8850                                    *
      *    ANALISTA....:   EDUARDO WATANABE        - BRQ               *
      *    DATA........:   11/12/2017                                  *
      *    PROJETO.....:   2017-0338395-5-003                          *
      *                    CAPTURA AUTOMATICA DE RESPONSABILIDADE SICOR*
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INSERIR DADOS DA COR0006R1 NA TABELA        *
      *                    RUECB046 (RESP_CONTR_RURAL)                 *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC87 - AREA DE COMUNICACAO COM O MODULO RUEC8850.       *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *================================================================*
BRQ001*----------------------------------------------------------------*
BRQ001* RECOMPILACAO PARA TRATAR A ALTERACAO NO BOOK DE RECEBIMENTO    *
BRQ001*----------------------------------------------------------------*
BRQ001*               U L T I M A  A L T E R A C A O                   *
BRQ001*----------------------------------------------------------------*
BRQ001*                                                                *
BRQ001*      ANALISTA     : RODOLFO VIEIRA        - BRQ                *
BRQ001*      DATA         : MAIO/2021                                  *
BRQ001*                                                                *
BRQ001*      OBJETIVOS    : INCLUIR NOVOS CAMPOS DO CATALOGO SICOR     *
BRQ001*                     V5.02 NA MENSAGEM COR0006R1                *
BRQ001*                                                                *
BRQ001*      REFERENCIA   : BRQ001                                     *
BRQ001*                                                                *
BRQ001*================================================================*
WIP001*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2022                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.05 - MVP1                *
.     *                    FORMATACAO DOS CAMPOS                       *
WIP001*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
STE001*---------------------------------------------------------------*
-     * * ALTERACAO * - SICOR 5.08 - ALTERACAO DO CAMPO CSIST-PROD    *
-     *---------------------------------------------------------------*
-     *   DATA  ....: OUTUBRO/2024                                    *
STE001*---------------------------------------------------------------*
7C0625*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: junho/2025                                *
.     * OBJETIVOS..........: FLASH - AGRO7-441 / AGRO7-442 / AGRO7-435 *
.     *                      ALTERAR PARA ENVIAR A SOLICITACAO DAS     *
.     *                      RESPONSABILIDADES DOS COOPERADOS (ATUAL)  *
.     *                      E TAMBEM DE TODOS OS CLIENTES             *
7C0625*================================================================*
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
           '*** INICIO DA WORKING RUEC8850 ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*
       77  WRK-INI                     PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-POS                     PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-FIM-RUECB046            PIC X(001)          VALUE SPACES.
       77  WRK-CONTRATO-BRADESCO       PIC X(001)          VALUE SPACES.

       01  WRK-CSEQ-RESP-RURAL         PIC S9(003) COMP-3  VALUE ZEROS.
       01  FILLER            REDEFINES WRK-CSEQ-RESP-RURAL.
           03  WRK-HEXA                PIC S9(003) COMP-3.

       01  WRK-VALOR                   PIC  X(019)         VALUE SPACES.
       01  FILLER            REDEFINES WRK-VALOR.
           03  FILLER                  PIC  X(004).
           03  WRK-VALOR-DB2           PIC  9(013)V99.

       01  WRK-AGENCIA-CEDULA          PIC  X(014)         VALUE SPACES.
       01  FILLER            REDEFINES WRK-AGENCIA-CEDULA.
           03  WRK-AGENCIA             PIC  9(005).
           03  WRK-CEDULA              PIC  9(009).

       01  WRK-CNPJ-X                  PIC  X(008)         VALUE SPACES.
       01  FILLER            REDEFINES WRK-CNPJ-X.
           03  WRK-CNPJ-9              PIC  9(008).

       01  WRK-NUM-REF-BACEN-X         PIC  X(011)         VALUE SPACES.
       01  FILLER            REDEFINES WRK-NUM-REF-BACEN-X.
           03  WRK-NUM-REF-BACEN-9     PIC  9(009).

       01  WRK-DATA-AMD.
           03  WRK-DATA-AAAA           PIC  X(004) VALUE SPACES.
           03  WRK-DATA-MM             PIC  X(002) VALUE SPACES.
           03  WRK-DATA-DD             PIC  X(002) VALUE SPACES.

       01  WRK-DATA-DB2.
           03  WRK-DATA-DD             PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '.'.
           03  WRK-DATA-MM             PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '.'.
           03  WRK-DATA-AAAA           PIC  X(004) VALUE SPACES.

       01  WRK-DTHR-BC.
           03  WRK-DTHR-AAAA           PIC  X(004) VALUE SPACES.
           03  WRK-DTHR-MM             PIC  X(002) VALUE SPACES.
           03  WRK-DTHR-DD             PIC  X(002) VALUE SPACES.
           03  WRK-DTHR-H              PIC  X(002) VALUE SPACES.
           03  WRK-DTHR-M              PIC  X(002) VALUE SPACES.
           03  WRK-DTHR-S              PIC  X(002) VALUE SPACES.

       01  WRK-DTHR-TIMESTAMP.
           03  WRK-DTHR-AAAA           PIC  X(004) VALUE SPACES.
           03                          PIC  X(001) VALUE '-'.
           03  WRK-DTHR-MM             PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '-'.
           03  WRK-DTHR-DD             PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '-'.
           03  WRK-DTHR-H              PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '.'.
           03  WRK-DTHR-M              PIC  X(002) VALUE SPACES.
           03                          PIC  X(001) VALUE '.'.
           03  WRK-DTHR-S              PIC  X(002) VALUE SPACES.
           03                          PIC  X(007) VALUE '.000000'.
       01  FILLER            REDEFINES WRK-DTHR-TIMESTAMP.
           03  WRK-DTHR-DB2            PIC  X(026).

BR0618 01  WRK-CTPO-PSSOA-COOP-N       PIC S9(04) COMP VALUE +0.
BR0618 01  WRK-CDOCTO-PSSOA-COOP-N     PIC S9(04) COMP VALUE +0.
BR0618 01  WRK-CTPO-PSSOA-BNEFC-N      PIC S9(04) COMP VALUE +0.
BR0618 01  WRK-CDOCTO-PSSOA-BNEFC-N    PIC S9(04) COMP VALUE +0.
BR4.12 01  WRK-COD-SUB-PROG-N          PIC S9(04) COMP VALUE +0.
RLOS   01  WRK-CCNPJ-BASE-MUTUA-N      PIC S9(04) COMP VALUE +0.
RLOS   01  WRK-CFLIAL-BASE-MUTUA-N     PIC S9(04) COMP VALUE +0.
RLOS   01  WRK-CCTRL-BASE-MUTUA-N      PIC S9(04) COMP VALUE +0.
RLOS   01  WRK-NREFT-BACEN-INVES-N     PIC S9(04) COMP VALUE +0.
RLOS   01  WRK-VPCELA-INVES-RURAL-N    PIC S9(04) COMP VALUE +0.
BR0618 01  WRK-CDOCTO-AUX.
BR0618     03  WRK-CDOCTO-AUX-N        PIC  9(14)      VALUE ZEROS.

       COPY 'I#RUEC86'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA FORMATACAO DE ERRO ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

           EXEC SQL INCLUDE RUECB09C END-EXEC.

           EXEC SQL INCLUDE RUFIB002 END-EXEC.

           EXEC SQL INCLUDE RUECB008 END-EXEC.

           EXEC SQL INCLUDE RUECB09B END-EXEC.

           EXEC SQL INCLUDE RUECB046 END-EXEC.


           EXEC SQL
               DECLARE CSR01-RUECB046 CURSOR WITH HOLD FOR
               SELECT  CAG_BCRIA
                    ,  CCDULA
                    ,  CSEQ_PSSOA
                    ,  CSEQ_RESP_RURAL
                FROM   DB2PRD.RESP_CONTR_RURAL
                WHERE  CAG_BCRIA       = :RUECB046.CAG-BCRIA
                  AND  CCDULA          = :RUECB046.CCDULA
                  AND  CSEQ_PSSOA      = :RUECB046.CSEQ-PSSOA
                  AND  CSEQ_RESP_RURAL >  0
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8850 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC87'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC87-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL  'CKRS1000'.

           MOVE ZEROS              TO RUEC87-COD-RET.

           PERFORM  1000-PROCESSAMENTO-PRINCIPAL.

           PERFORM  4000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAMENTO-PRINCIPAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

           IF  RUEC87-COD-RET  EQUAL  ZEROS
               PERFORM 3000-SELECT-RUECB09C
           END-IF.

           IF  RUEC87-COD-RET  EQUAL  ZEROS
               PERFORM 3100-SELECT-RUECB008
           END-IF.

           IF  RUEC87-COD-RET  EQUAL  ZEROS
               PERFORM 3400-PROCESSAR-SISMSG
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC87-PGM-CHAMADOR EQUAL SPACES
               MOVE 01             TO RUEC87-COD-RET
               MOVE 'PGM CHAMADOR INVALIDO'
                                   TO RUEC87-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF  RUEC87-NRO-CTRLIF   EQUAL SPACES
               MOVE 01             TO RUEC87-COD-RET
               MOVE 'CAMPO NAO PREENCHIDO - INFORMAR NRO-CTRLIF'
                                   TO RUEC87-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-SELECT-RUECB09C SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                 TO NMNTRC-MSGEM-RURAL OF RUECB09C.
           MOVE 'COR0006'             TO CMSGEM-RECOR       OF RUECB09C.
           MOVE RUEC87-NRO-CTRLIF     TO CCTRL-RQUIS-FINCR  OF RUECB09C.

           EXEC SQL
               SELECT   WCONTD_MSGEM_RURAL
               INTO     :RUECB09C.WCONTD-MSGEM-RURAL
                 FROM DB2PRD.TMNTRC_MSGEM_RURAL
                WHERE NMNTRC_MSGEM_RURAL >= :RUECB09C.NMNTRC-MSGEM-RURAL
                  AND CMSGEM_RECOR       =  :RUECB09C.CMSGEM-RECOR
                  AND CCTRL_RQUIS_FINCR  =  :RUECB09C.CCTRL-RQUIS-FINCR
                  WITH UR
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC87-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC87-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'TMNTRC_MSGEM_RURAL' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC87-COD-RET
               PERFORM 4000-ROTINA-FIM
           ELSE
               MOVE WCONTD-MSGEM-RURAL-TEXT (30:14)
                                        TO WRK-AGENCIA-CEDULA
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-SELECT-RUECB008 SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-AGENCIA             TO CJUNC-DEPDC OF RUECB008
           MOVE WRK-CEDULA              TO CCDULA      OF RUECB008.

           EXEC SQL
               SELECT   CJUNC_DEPDC,
                        CCDULA,
                        CSEQ_PSSOA
               INTO     :RUECB008.CJUNC-DEPDC,
                        :RUECB008.CCDULA,
                        :RUECB008.CSEQ-PSSOA
                 FROM DB2PRD.PSSOA_CDULA_TEMPR
                WHERE CJUNC_DEPDC     = :RUECB008.CJUNC-DEPDC
                  AND CCDULA          = :RUECB008.CCDULA
                  AND CTPO_RESP_PSSOA = 'T'
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC87-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC87-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'PSSOA_CDULA_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0020'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 03                  TO RUEC87-COD-RET
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3400-PROCESSAR-SISMSG           SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-CSEQ-RESP-RURAL.

           COMPUTE WRK-INI = 1 + LENGTH OF RUEC86-COR0006R1-1.

           PERFORM VARYING WRK-POS FROM WRK-INI
              BY LENGTH OF RUEC86-GRUPO-COR0006R1-OP
                     UNTIL WRK-POS GREATER 30000
                        OR RUEC87-SISMSG (WRK-POS:6) NOT EQUAL '{1000}'
           END-PERFORM.

           IF  WRK-POS             GREATER 30000
               MOVE 01             TO RUEC87-COD-RET
               MOVE 'ERRO EM SISMSG - NAO ENCONTROU <DTHRBC>'
                                   TO RUEC87-MENSAGEM
               GO  TO  3400-99-FIM
           END-IF.

           MOVE RUEC87-SISMSG (WRK-POS:14) TO WRK-DTHR-BC.
           MOVE CORR WRK-DTHR-BC       TO WRK-DTHR-TIMESTAMP.

           PERFORM 3410-INSERT-RUECB046 VARYING WRK-POS FROM WRK-INI
              BY LENGTH OF RUEC86-GRUPO-COR0006R1-OP
                     UNTIL WRK-POS GREATER 30000
                        OR RUEC87-SISMSG (WRK-POS:6) NOT EQUAL '{1000}'.

      *---------------------------------------------------------------*
       3400-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3410-INSERT-RUECB046           SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC87-SISMSG
                (WRK-POS : LENGTH OF RUEC86-GRUPO-COR0006R1-OP)
                                       TO RUEC86-GRUPO-COR0006R1-OP.

           ADD  1                      TO WRK-CSEQ-RESP-RURAL.

           INITIALIZE RUECB046.

           MOVE CJUNC-DEPDC OF RUECB008 TO CAG-BCRIA        OF RUECB046.
           MOVE CCDULA      OF RUECB008 TO CCDULA           OF RUECB046.
           MOVE CSEQ-PSSOA  OF RUECB008 TO CSEQ-PSSOA       OF RUECB046.
           MOVE WRK-HEXA                TO CSEQ-RESP-RURAL  OF RUECB046.

           PERFORM 3500-VERIFICA-RUECB09B.

           IF WRK-CONTRATO-BRADESCO     EQUAL 'S'
              MOVE 237                  TO CBCO             OF RUECB046
              MOVE CCDULA   OF RUFIB002 TO CEMPTM-RURAL     OF RUECB046
           ELSE
              MOVE 999                  TO CBCO             OF RUECB046
           END-IF.

           MOVE RUEC86-NUM-REF-BCCOR    TO CREFT-RECOR      OF RUECB046.
           MOVE RUEC86-NUM-ORDEM-DESTC-OR
                                        TO NORD-DSTNO-ORCAM OF RUECB046.

           MOVE RUEC86-DT-EMS        TO WRK-DATA-AMD
           MOVE CORR WRK-DATA-AMD    TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2         TO DCDULA-CREDT-RURAL OF RUECB046.

           MOVE RUEC86-DT-VENC       TO WRK-DATA-AMD
           MOVE CORR WRK-DATA-AMD    TO WRK-DATA-DB2
           MOVE WRK-DATA-DB2         TO DVCTO-CREDT-RURAL  OF RUECB046.

           IF RUEC86-COD-PROG-LINHA-CRED
                                        EQUAL ALL '*'
              MOVE SPACES               TO CPROG-RURAL      OF RUECB046
           ELSE
              MOVE RUEC86-COD-PROG-LINHA-CRED
                                        TO CPROG-RURAL      OF RUECB046
           END-IF.

           MOVE RUEC86-TP-FNTE-REC      TO CTPO-FONTE-RECOR OF RUECB046.
           MOVE RUEC86-COD-ENPNMNT      TO CEMPTO-BACEN     OF RUECB046.
PRI499     MOVE RUEC86-COD-SIST-PRODC
WIP001                                  TO CSIST-PROD       OF RUECB046.

           IF RUEC86-COD-MOD-SEGR       IS NUMERIC
              MOVE RUEC86-COD-MOD-SEGR
                                     TO CMODLD-SEGUR-RURAL  OF RUECB046
           END-IF.

           MOVE RUEC86-COD-MUNIC        TO CMUN             OF RUECB046.

           MOVE RUEC86-VLR-PARCL-CRED   TO WRK-VALOR
           MOVE WRK-VALOR-DB2           TO VFINAN           OF RUECB046
                                           VPCELA           OF RUECB046.

           IF RUEC86-VLR-PARCL-REC-PROPRIO
                                        IS NUMERIC
              MOVE RUEC86-VLR-PARCL-REC-PROPRIO
                                        TO WRK-VALOR
              MOVE WRK-VALOR-DB2        TO VPCELA-REC-PPRIO OF RUECB046
           END-IF.

           IF RUEC86-VLR-PARCL-MANUT-FAMLR
                                        IS NUMERIC
              MOVE RUEC86-VLR-PARCL-MANUT-FAMLR
                                        TO WRK-VALOR
              MOVE WRK-VALOR-DB2     TO VPCELA-MANUT-FAMLR  OF RUECB046
           END-IF.

           IF RUEC86-VLR-PARCL-COOPERD  IS NUMERIC
              MOVE RUEC86-VLR-PARCL-COOPERD
                                        TO WRK-VALOR
              MOVE WRK-VALOR-DB2     TO VPCELA-COOP-RURAL   OF RUECB046
           END-IF.

           IF RUEC86-COD-PROG-LINHA-CRED-COO
                                        EQUAL ALL '*'
              MOVE SPACES               TO CPROG-CREDT-COOP OF RUECB046
           ELSE
              MOVE RUEC86-COD-PROG-LINHA-CRED-COO
                                        TO CPROG-CREDT-COOP OF RUECB046
           END-IF.

BR0618     IF RUEC86-TPSSOA-COOP        EQUAL ALL '*'
BR0618        MOVE SPACES               TO CTPO-PSSOA-COOP  OF RUECB046
BR0618        MOVE -1                   TO WRK-CTPO-PSSOA-COOP-N
BR0618     ELSE
BR0618        MOVE ZEROS                TO WRK-CTPO-PSSOA-COOP-N
BR0618        MOVE RUEC86-TPSSOA-COOP   TO CTPO-PSSOA-COOP  OF RUECB046
BR0618     END-IF.

BR0618     IF RUEC86-CNPJ-CPF-COOP      IS NUMERIC
BR0618        MOVE ZEROS                TO WRK-CDOCTO-PSSOA-COOP-N
BR0618        MOVE RUEC86-CNPJ-CPF-COOP TO WRK-CDOCTO-AUX-N
BR0618        MOVE WRK-CDOCTO-AUX       TO CDOCTO-PSSOA-COOP OF RUECB046
BR0618     ELSE
BR0618        MOVE SPACES               TO CDOCTO-PSSOA-COOP OF RUECB046
BR0618        MOVE -1                   TO WRK-CDOCTO-PSSOA-COOP-N
BR0618     END-IF.

BR0618     IF RUEC86-TPBENEFC           EQUAL ALL '*'
BR0618        MOVE SPACES               TO CTPO-PSSOA-BNEFC OF RUECB046
BR0618        MOVE -1                   TO WRK-CTPO-PSSOA-BNEFC-N
BR0618     ELSE
BR0618        MOVE ZEROS                TO WRK-CTPO-PSSOA-BNEFC-N
BR0618        MOVE RUEC86-TPBENEFC      TO CTPO-PSSOA-BNEFC OF RUECB046
BR0618     END-IF.

BR0618     IF RUEC86-CNPJ-CPF-BENEF     IS NUMERIC
BR0618        MOVE ZEROS                TO WRK-CDOCTO-PSSOA-BNEFC-N
BR0618        MOVE RUEC86-CNPJ-CPF-BENEF
BR0618                                  TO WRK-CDOCTO-AUX-N
BR0618        MOVE WRK-CDOCTO-AUX       TO CDOCTO-PSSOA-BNEFC
BR0618                                                       OF RUECB046
BR0618     ELSE
BR0618        MOVE SPACES               TO CDOCTO-PSSOA-BNEFC
BR0618                                                       OF RUECB046
BR0618        MOVE -1                   TO WRK-CDOCTO-PSSOA-BNEFC-N
BR0618     END-IF.

BR4.12     IF RUEC86-COD-SUB-PROG       EQUAL ALL '*'
BR4.12        MOVE SPACES               TO CSUB-PROG-RURAL  OF RUECB046
BR4.12        MOVE -1                   TO WRK-COD-SUB-PROG-N
BR4.12     ELSE
BR4.12        MOVE ZEROS                TO WRK-COD-SUB-PROG-N
BR4.12        MOVE RUEC86-COD-SUB-PROG  TO CSUB-PROG-RURAL  OF RUECB046
BR4.12     END-IF.

RLOS       IF RUEC86-CNPJ-PROAGRO       EQUAL ALL '*'
           OR RUEC86-CNPJ-PROAGRO       EQUAL LOW-VALUES
.             MOVE ZEROS                TO CCNPJ-BASE-MUTUA  OF RUECB046
.             MOVE -1                   TO WRK-CCNPJ-BASE-MUTUA-N
.          ELSE
.             MOVE ZEROS                TO WRK-CCNPJ-BASE-MUTUA-N
.             MOVE RUEC86-CNPJ-PROAGRO  TO WRK-CNPJ-X
.             MOVE WRK-CNPJ-9           TO CCNPJ-BASE-MUTUA  OF RUECB046
.          END-IF
.
.          MOVE -1                      TO WRK-CFLIAL-BASE-MUTUA-N
.                                          WRK-CCTRL-BASE-MUTUA-N.
.
.          IF RUEC86-NUM-REF-PROAGRO    EQUAL ALL '*'
.          OR RUEC86-NUM-REF-PROAGRO    EQUAL LOW-VALUES
.             MOVE ZEROS                TO NREFT-BACEN-INVES OF RUECB046
.             MOVE -1                   TO WRK-NREFT-BACEN-INVES-N
.          ELSE
.             MOVE ZEROS                TO WRK-NREFT-BACEN-INVES-N
.             MOVE RUEC86-NUM-REF-PROAGRO
                                        TO WRK-NUM-REF-BACEN-X
.             MOVE WRK-NUM-REF-BACEN-9  TO NREFT-BACEN-INVES OF RUECB046
.          END-IF.
.
.          IF RUEC86-VLR-PCL-PROAGRO    EQUAL ALL '*'
.          OR RUEC86-VLR-PCL-PROAGRO    EQUAL LOW-VALUES
.             MOVE ZEROS                TO VPCELA-INVES-RURAL
.                                                            OF RUECB046
.             MOVE -1                   TO WRK-VPCELA-INVES-RURAL-N
.          ELSE
.             MOVE ZEROS                TO WRK-VPCELA-INVES-RURAL-N
.             MOVE RUEC86-VLR-PCL-PROAGRO
                                        TO WRK-VALOR
.             MOVE WRK-VALOR-DB2        TO VPCELA-INVES-RURAL
.                                                            OF RUECB046
RLOS       END-IF.

           EXEC SQL
                INSERT INTO DB2PRD.RESP_CONTR_RURAL
                     (CAG_BCRIA
                    , CCDULA
                    , CSEQ_PSSOA
                    , CSEQ_RESP_RURAL
                    , CBCO
                    , CEMPTM_RURAL
                    , CFNALD_RURAL
                    , QDIA_EMPR
                    , VFINAN
                    , CPRODT_CREDT_RURAL
                    , DCDULA_CREDT_RURAL
                    , DVCTO_CREDT_RURAL
                    , CCONTR_EMPTM_EXTER
                    , IPRODT_EXTER
                    , RFNALD_EMPTM_EXTER
                    , CEMPTO_BACEN
                    , CREFT_RECOR
                    , NORD_DSTNO_ORCAM
                    , CPROG_RURAL
                    , CTPO_FONTE_RECOR
                    , CSIST_PROD
                    , CMODLD_SEGUR_RURAL
                    , CMUN
                    , VPCELA
                    , VPCELA_REC_PPRIO
                    , VPCELA_MANUT_FAMLR
                    , VPCELA_COOP_RURAL
                    , CPROG_CREDT_COOP
                    , HATULZ
                    , CTPO_PSSOA_COOP
                    , CDOCTO_PSSOA_COOP
                    , CTPO_PSSOA_BNEFC
                    , CDOCTO_PSSOA_BNEFC
                    , CSUB_PROG_RURAL
RLOS                , CCNPJ_BASE_MUTUA
.                   , CFLIAL_BASE_MUTUA
.                   , CCTRL_BASE_MUTUA
.                   , NREFT_BACEN_INVES
RLOS                , VPCELA_INVES_RURAL
                    )
                VALUES
                     (:RUECB046.CAG-BCRIA
                    , :RUECB046.CCDULA
                    , :RUECB046.CSEQ-PSSOA
                    , :RUECB046.CSEQ-RESP-RURAL
                    , :RUECB046.CBCO
                    , :RUECB046.CEMPTM-RURAL
                    , :RUECB046.CFNALD-RURAL
                    , :RUECB046.QDIA-EMPR
                    , :RUECB046.VFINAN
                    , :RUECB046.CPRODT-CREDT-RURAL
                    , :RUECB046.DCDULA-CREDT-RURAL
                    , :RUECB046.DVCTO-CREDT-RURAL
                    , :RUECB046.CCONTR-EMPTM-EXTER
                    , :RUECB046.IPRODT-EXTER
                    , :RUECB046.RFNALD-EMPTM-EXTER
                    , :RUECB046.CEMPTO-BACEN
                    , :RUECB046.CREFT-RECOR
                    , :RUECB046.NORD-DSTNO-ORCAM
                    , :RUECB046.CPROG-RURAL
                    , :RUECB046.CTPO-FONTE-RECOR
                    , :RUECB046.CSIST-PROD
                    , :RUECB046.CMODLD-SEGUR-RURAL
                    , :RUECB046.CMUN
                    , :RUECB046.VPCELA
                    , :RUECB046.VPCELA-REC-PPRIO
                    , :RUECB046.VPCELA-MANUT-FAMLR
                    , :RUECB046.VPCELA-COOP-RURAL
                    , :RUECB046.CPROG-CREDT-COOP
                    , :WRK-DTHR-DB2
                    , :RUECB046.CTPO-PSSOA-COOP
                           :WRK-CTPO-PSSOA-COOP-N
                    , :RUECB046.CDOCTO-PSSOA-COOP
                           :WRK-CDOCTO-PSSOA-COOP-N
                    , :RUECB046.CTPO-PSSOA-BNEFC
                           :WRK-CTPO-PSSOA-BNEFC-N
                    , :RUECB046.CDOCTO-PSSOA-BNEFC
                           :WRK-CDOCTO-PSSOA-BNEFC-N
                    , :RUECB046.CSUB-PROG-RURAL
                           :WRK-COD-SUB-PROG-N
RLOS                , :RUECB046.CCNPJ-BASE-MUTUA
.                          :WRK-CCNPJ-BASE-MUTUA-N
.                   , :RUECB046.CFLIAL-BASE-MUTUA
.                          :WRK-CFLIAL-BASE-MUTUA-N
.                   , :RUECB046.CCTRL-BASE-MUTUA
.                          :WRK-CCTRL-BASE-MUTUA-N
.                   , :RUECB046.NREFT-BACEN-INVES
.                          :WRK-NREFT-BACEN-INVES-N
RLOS                , :RUECB046.VPCELA-INVES-RURAL
                           :WRK-VPCELA-INVES-RURAL-N)
           END-EXEC.

           IF (SQLCODE           NOT EQUAL   +0)    OR
              (SQLWARN0              EQUAL  'W')
               MOVE 'DB2'                    TO  ERR-TIPO-ACESSO
               MOVE 'RESP_CONTR_RURAL'       TO  ERR-DBD-TAB
               MOVE 'INSERT  '               TO  ERR-FUN-COMANDO
               MOVE SQLCODE                  TO  ERR-SQL-CODE
               MOVE '0070'                   TO  ERR-LOCAL
               MOVE SPACES                   TO  ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

      *---------------------------------------------------------------*
       3410-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3500-VERIFICA-RUECB09B      SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                 TO CCONTR-CREDT-RURAL OF RUECB09B.
           MOVE ZEROS                 TO NENVIO-RECOR       OF RUECB09B.
           MOVE '0001-01-01-00.00.00.000000'
                                      TO HULT-ATULZ         OF RUECB09B.
           MOVE RUEC86-NUM-REF-BCCOR  TO CREFT-RECOR        OF RUECB09B.

           EXEC SQL
               SELECT   CCONTR_CREDT_RURAL
               INTO     :RUECB09B.CCONTR-CREDT-RURAL
                 FROM DB2PRD.THIST_ENVIO_RECOR
                WHERE CCONTR_CREDT_RURAL >= :RUECB09B.CCONTR-CREDT-RURAL
                  AND NENVIO_RECOR       >= :RUECB09B.NENVIO-RECOR
                  AND HULT_ATULZ         >= :RUECB09B.HULT-ATULZ
                  AND CREFT_RECOR        =  :RUECB09B.CREFT-RECOR
                  FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC87-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC87-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0080'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 'N'                 TO WRK-CONTRATO-BRADESCO
           ELSE
               MOVE 'S'                 TO WRK-CONTRATO-BRADESCO
               PERFORM 3600-SELECT-RUFIB002
           END-IF.

      *---------------------------------------------------------------*
       3500-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3600-SELECT-RUFIB002 SECTION.
      *---------------------------------------------------------------*

           MOVE CCONTR-CREDT-RURAL OF RUECB09B TO CCONTR OF RUFIB002.

           EXEC SQL
               SELECT   CJUNC_DEPDC,
                        CCDULA
               INTO     :RUFIB002.CJUNC-DEPDC,
                        :RUFIB002.CCDULA
                 FROM DB2PRD.CONTRATO_RURAL
                WHERE CCONTR   = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE             NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 99                  TO RUEC87-COD-RET
               MOVE 'ERRO ACESSO DB2'   TO RUEC87-MENSAGEM
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'    TO ERR-DBD-TAB
               MOVE 'SELECT'            TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '0090'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 4000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE ZEROS               TO CJUNC-DEPDC OF RUFIB002
                                           CCDULA      OF RUFIB002
           END-IF.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           IF  RUEC87-COD-RET      EQUAL 99
               MOVE 'RUEC8850'     TO ERR-MODULO
               MOVE ERRO-AREA      TO RUEC87-ERRO-AREA
               MOVE SQLCA(1:136)   TO RUEC87-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                EXIT.
      *---------------------------------------------------------------**
