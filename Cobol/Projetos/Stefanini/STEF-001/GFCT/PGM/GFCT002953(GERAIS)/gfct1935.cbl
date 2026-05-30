      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
CPMCM *
       PROGRAM-ID. GFCT1935.
       AUTHOR.     WINISSIUS LUIZ JANCZESKI.
      *================================================================*
      *                    C P M - S I S T E M A S                     *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1935                                    *
      *    PROGRAMADOR.:   WINISSIUS LUIZ JANCZESKI - CPM PATO BRANCO  *
      *    ANALISTA CPM:   FELIPE SOMMER            - CPM PATO BRANCO  *
      *    ANALISTA....:   MARCELO CREMM            - PROCKWORK/ GP. 50*
      *    DATA........:   24/08/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUA TRAG DE AGENCIAS E CONTAS DA BASE DE *
      *        DADOS DE MOVIMENTO CORRENTISTA DE TARIFA. EFETUA TAMBEM *
      *        TRATAMENTO PARA RESTART-COMMIT.                         *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.CTRL_ROTNA_GFCT      GFCTB000        *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT    GFCTB092        *
      *                    DB2PRD.PDIDO_ESTRN_CTA      GFCTB0A5        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  RESTARTI                    I#GFCTU3          *
      *                  RESTARTO                    I#GFCTU3          *
      *                  ARQUTRAG                    I#TRAGAE          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - MODULO PARA CANCELAMENTO DO PROGRAMA             *
      *    POOL0160 - OBTEM JOB E FAC                                  *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT RESTARTI  ASSIGN     TO UT-S-RESTARTI
           FILE STATUS                 IS WRK-FS-RESTARTI.

           SELECT RESTARTO  ASSIGN     TO UT-S-RESTARTO
           FILE STATUS                 IS WRK-FS-RESTARTO.

           SELECT ARQUTRAG  ASSIGN     TO UT-S-ARQUTRAG
           FILE STATUS                 IS WRK-FS-ARQUTRAG.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO PARA RESTART - CONTEM DADOS DA TAB. GFCTB000*
      *            ORG. SEQUENCIAL     -   LRECL   =   070             *
      *----------------------------------------------------------------*

       FD  RESTARTI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTU3.

      *----------------------------------------------------------------*
      *    INPUT: ARQUIVO COM TODAS CONTAS TRAGUEADAS DO DIA           *
      *            ORG. SEQUENCIAL     -   LRECL   =   077             *
      *----------------------------------------------------------------*

       FD  ARQUTRAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#TRAGAE.

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO COM CONTAS TRAGUEADAS E ENCONTRADAS DO GFCT *
      *            ORG. SEQUENCIAL     -   LRECL   =   070             *
      *----------------------------------------------------------------*

       FD  RESTARTO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTU3.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ARQUTRAG          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-RESTARTI          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-TAB               PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-GFCTB000          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-GFCTB000       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-ATUALIZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-OCORRENCIAS             PIC  9(007) COMP-3  VALUE ZEROS.
       77  ACU-OCORR-AGENCIAS          PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-RESTART                 PIC  X(001)         VALUE SPACES.
       77  WRK-QREG-COMIT              PIC  ZZZ.ZZ9        VALUE ZEROS.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-MASK-QREG               PIC  ZZZZ9          VALUE ZEROS.
       77  WRK-MASK-AGE                PIC  ZZZZ9          VALUE ZEROS.
       77  WRK-MASK-CTA                PIC  ZZZZZZ9        VALUE ZEROS.
       77  WRK-HAVIA-ESTORNO           PIC  X(001)         VALUE SPACES.

       01  WRK-GFCTB092.
           05 WRK-DENVIO-MOVTO-TARIF PIC  X(010)           VALUE SPACES.
           05 WRK-CROTNA-ORIGE-MOVTO PIC  X(004)           VALUE SPACES.
           05 WRK-CNRO-ARQ-MOVTO     PIC S9(002)    COMP-3 VALUE ZEROS.
           05 WRK-CSEQ-MOVTO         PIC S9(011)    COMP-3 VALUE ZEROS.
           05 WRK-CSERVC-TARIF       PIC S9(005)    COMP-3 VALUE ZEROS.
           05 WRK-CMIDIA-SERVC-MOVTO PIC S9(005)    COMP-3 VALUE ZEROS.
           05 WRK-CPERIF-SERVC-MOVTO PIC S9(005)    COMP-3 VALUE ZEROS.
           05 WRK-QEVNTO-AGRUP       PIC S9(006)    COMP-3 VALUE ZEROS.
           05 WRK-HPREST-SERVC-MOVTO PIC  X(008)           VALUE SPACES.
           05 WRK-CTPO-CTA-MOVTO     PIC S9(001)    COMP-3 VALUE ZEROS.
           05 WRK-CBCO-DSTNO-MOVTO   PIC S9(003)    COMP-3 VALUE ZEROS.
           05 WRK-CAG-DSTNO-MOVTO    PIC S9(005)    COMP-3 VALUE ZEROS.
           05 WRK-CCTA-DSTNO-MOVTO   PIC S9(013)    COMP-3 VALUE ZEROS.
           05 WRK-CPAB-DSTNO-MOVTO   PIC S9(005)    COMP-3 VALUE ZEROS.
           05 WRK-CAG-CTLZA-DEB      PIC S9(005)    COMP-3 VALUE ZEROS.
           05 WRK-CCTA-CTLZA-DEB     PIC S9(013)    COMP-3 VALUE ZEROS.
           05 WRK-VTARIF-BRUTO-MOVTO PIC S9(009)V99 COMP-3 VALUE ZEROS.
           05 WRK-VTARIF-LIQ-MOVTO   PIC S9(009)V99 COMP-3 VALUE ZEROS.
           05 WRK-VTARIF-DEB-MOVTO   PIC S9(009)V99 COMP-3 VALUE ZEROS.
           05 WRK-DAGNDA-DEB-MOVTO   PIC  X(010)           VALUE SPACES.
           05 WRK-DEFETV-DEB-MOVTO   PIC  X(010)           VALUE SPACES.
           05 WRK-QMAX-TENTV-DEB     PIC S9(003)    COMP-3 VALUE ZEROS.
           05 WRK-QTENTV-DEB-MOVTO   PIC S9(003)    COMP-3 VALUE ZEROS.
           05 WRK-CSIT-EVNTO-RECBD   PIC S9(002)    COMP-3 VALUE ZEROS.
           05 WRK-CIDTFD-PGMC-COBR   PIC S9(002)    COMP-3 VALUE ZEROS.
           05 WRK-DOCOR-EVNTO        PIC  X(010)           VALUE SPACES.
           05 WRK-VTARIF-CADTR-SIST  PIC S9(009)99  COMP-3 VALUE ZEROS.
           05 WRK-CINDCD-TARIF-PCELD PIC  X(001)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES *'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-QUEBRA.
           05 WRK-AGENCIA-QUEBRA       PIC 9(005)          VALUE ZEROS.
           05 WRK-CONTA-QUEBRA         PIC 9(007)          VALUE ZEROS.

       01  WRK-CHAVE-ARQTRAG.
           05 WRK-AGENCIA-ANT          PIC 9(005)          VALUE ZEROS.
           05 WRK-CONTA-ANT            PIC 9(007)          VALUE ZEROS.

       01  WRK-CHAVE-BL.
           05 WRK-AGENCIA-BL           PIC 9(005)          VALUE ZEROS.
           05 WRK-CONTA-BL             PIC 9(007)          VALUE ZEROS.

       01  WRK-REST-CHAVE.
           05  WRK-REST-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  WRK-REST-CONTA          PIC  9(007)         VALUE ZEROS.
           05  WRK-REST-LIDOS-TAB      PIC  9(009)         VALUE ZEROS.
           05  WRK-REST-ATUALIZADOS    PIC  9(009)         VALUE ZEROS.
           05  FILLER                  PIC  X(220)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTES DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-RESTARTI             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-RESTARTO             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQUTRAG             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *----------------------------------------------------------------*

       01  WRK-MSG01                   PIC  X(075)         VALUE
           '** FIM DE RESTARTI ANTES DO FIM DO RESTART **'.
       01  WRK-MSG02                   PIC  X(075)         VALUE
           '** CANCELADO APOS O PRIMEIRO COMMIT **'.
       01  WRK-MSG03                   PIC  X(075)         VALUE
           '** QUANTIDADE DE OCORRENCIAS ENVIADO PELO PARM, PARA COMMIT,
      -    ' INVALIDO **'.
       01  WRK-MSG04                   PIC  X(075)         VALUE
           '** BRECA ENVIADO PELO PARM, DIFERENTE DE "S" E "N" **'.
       01  WRK-MSG05                   PIC  X(075)         VALUE
           '** ARQUIVO RESTARTI VAZIO E EXISTE RESTART **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS DE NULIDADE *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB000-NULL.
           05 WRK-CUSUAR-SENHA-NULL    PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-HATULZ-NULL          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-WCHAVE-RESTT-NULL    PIC S9(004) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL0160 *'.
      *----------------------------------------------------------------*

       01  WRK-JOBNAME-0160          PIC  X(008)           VALUE SPACES.
       01  WRK-VALORFAC-0160         PIC  9(005)V99 COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB000
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A5
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB092 CURSOR WITH HOLD FOR
               SELECT
                   DENVIO_MOVTO_TARIF,
                   CROTNA_ORIGE_MOVTO,
                   CNRO_ARQ_MOVTO,
                   CSEQ_MOVTO,
                   CSERVC_TARIF,
                   CMIDIA_SERVC_MOVTO,
                   CPERIF_SERVC_MOVTO,
                   QEVNTO_AGRUP,
                   HPREST_SERVC_MOVTO,
                   CTPO_CTA_MOVTO,
                   CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CPAB_DSTNO_MOVTO,
                   CAG_CTLZA_DEB,
                   CCTA_CTLZA_DEB,
                   VTARIF_BRUTO_MOVTO,
                   VTARIF_LIQ_MOVTO,
                   VTARIF_DEB_MOVTO,
                   DAGNDA_DEB_MOVTO,
                   DEFETV_DEB_MOVTO,
                   QMAX_TENTV_DEB,
                   QTENTV_DEB_MOVTO,
                   CSIT_EVNTO_RECBD,
                   CIDTFD_PGMC_COBR,
                   DOCOR_EVNTO,
                   VTARIF_CADTR_SIST,
                   CINDCD_TARIF_PCELD
             FROM  DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                  ((CAG_DSTNO_MOVTO   = :GFCTB092.CAG-DSTNO-MOVTO    AND
                    CCTA_DSTNO_MOVTO  > :GFCTB092.CCTA-DSTNO-MOVTO )  OR
                   (CAG_DSTNO_MOVTO   > :GFCTB092.CAG-DSTNO-MOVTO )) AND
                    CSIT_EVNTO_RECBD IN (1, 2, 3, 9, 13, 15, 17)
             ORDER BY CAG_DSTNO_MOVTO,
                      CCTA_DSTNO_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-AREA-PARM.
           05  LNK-TAMA-PARM           PIC S9(004) COMP.
           05  LNK-QTDE-COMMIT         PIC  9(007).
*********  05  LNK-BRECA               PIC  X(001).

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-AREA-PARM.
      *================================================================*

      ******************************************************************
      *INICIO DO PROGRAMA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INICIALIZAR                                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  LNK-QTDE-COMMIT         NOT NUMERIC OR
               LNK-QTDE-COMMIT         EQUAL ZEROS
               DISPLAY '*************** GFCT1935 ****************'
               DISPLAY '*                                       *'
               DISPLAY '*    QUANTIDADE DE OCORRENCIAS ENVIADO  *'
               DISPLAY '*    PELO PARM, PARA COMMIT, INVALIDO.  *'
               DISPLAY '*                                       *'
               DISPLAY '*    QUANTIDADE: ' LNK-QTDE-COMMIT
                                               '                *'
               DISPLAY '*                                       *'
               DISPLAY '*          PROGRAMA CANCELADO           *'
               DISPLAY '*                                       *'
               DISPLAY '*****************************************'
               MOVE WRK-MSG03          TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

*********  IF  LNK-BRECA               NOT EQUAL 'S' AND 'N'
*********      DISPLAY '*************** GFCT1935 ****************'
*********      DISPLAY '*                                       *'
*********      DISPLAY '*    BRECA ENVIADO PELO PARM,           *'
*********      DISPLAY '*    DIFERENTE DE "S" E "N"             *'
*********      DISPLAY '*                                       *'
*********      DISPLAY '*    BRECA: ' LNK-BRECA
*********                           '                           *'
*********      DISPLAY '*                                       *'
*********      DISPLAY '*          PROGRAMA CANCELADO           *'
*********      DISPLAY '*                                       *'
*********      DISPLAY '*****************************************'
*********      MOVE WRK-MSG04          TO ERR-TEXTO
*********      PERFORM 9999-PROCESSAR-ROTINA-ERRO
*********  END-IF.

           CALL 'POOL1050'.

           OPEN INPUT  RESTARTI
                       ARQUTRAG
                OUTPUT RESTARTO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTAR FILE STATUS DOS ARQUIVOS                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-RESTARTI.

           PERFORM 1120-TESTAR-FS-RESTARTO.

           PERFORM 1130-TESTAR-FS-ARQUTRAG.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTAR FILE STATUS DO ARQUIVO RESTARTI                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-RESTARTI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RESTARTI         NOT EQUAL '00'
               MOVE 'RESTARTI'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RESTARTI    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTAR FILE STATUS DO ARQUIVO RESTARTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-RESTARTO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RESTARTO         NOT EQUAL '00'
               MOVE 'RESTARTO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RESTARTO    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTAR FILE STATUS DO ARQUTRAG                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ARQUTRAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQUTRAG         NOT EQUAL '00'
               MOVE 'ARQUTRAG'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQUTRAG    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR VAZIO DO ARQUIVO ARQUTRAG                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ARQUTRAG.

           IF  ACU-LIDOS-ARQUTRAG      EQUAL ZEROS
               DISPLAY '*********** GFCT1935 ***********'
               DISPLAY '*                              *'
               DISPLAY '*  ARQUIVO TRAG DE MOVTO VAZIO *'
               DISPLAY '*         TERMINO NORMAL       *'
               DISPLAY '*                              *'
               DISPLAY '********************************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2200-OBTER-PARAMETRO.

           PERFORM 2300-TRATAR-RESTART.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LEITURA DO ARQUIVO DE ENTRADA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ARQUTRAG               SECTION.
      *----------------------------------------------------------------*

           READ ARQUTRAG.

           IF  WRK-FS-ARQUTRAG         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-ARQUTRAG.

           MOVE AGEVTRAN               TO WRK-AGENCIA-ANT.
           MOVE CTAVTRAN               TO WRK-CONTA-ANT.

           ADD 1                       TO ACU-LIDOS-ARQUTRAG.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER PARAMETRO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-OBTER-PARAMETRO            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR RESTART                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-TRATAR-RESTART             SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ACESSAR-GFCTB000.

           IF  WRK-RESTART             EQUAL 'S'
               IF  QREG-COMIT          OF GFCTB000 NOT EQUAL ZEROS
                   PERFORM 2320-VER-VAZIO-RESTARTI
                   PERFORM 2330-PROCS-RESTARTO
                     UNTIL ACU-GRAVADOS EQUAL QREG-COMIT OF GFCTB000 OR
                       WRK-FS-RESTARTI EQUAL '10'
                   IF  ACU-GRAVADOS     LESS QREG-COMIT  OF GFCTB000
                       PERFORM 2340-MONTAR-DISP-FIM-RESTARTI
                   END-IF
                   PERFORM 2350-MONTAR-DISP-REST
               ELSE
                   MOVE ZEROS          TO CAG-DSTNO-MOVTO  OF GFCTB092
                                          CCTA-DSTNO-MOVTO OF GFCTB092
               END-IF
           ELSE
               MOVE ZEROS              TO CAG-DSTNO-MOVTO  OF GFCTB092
                                          CCTA-DSTNO-MOVTO OF GFCTB092
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VER SE EXISTE REGISTRO DE RESTART                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ACESSAR-GFCTB000           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTB000.

           CALL 'POOL0160'             USING WRK-JOBNAME-0160
                                             WRK-VALORFAC-0160.

           MOVE 'GFCT1935'             TO CPROG             OF GFCTB000.
           MOVE WRK-JOBNAME-0160       TO CJOB              OF GFCTB000.
           MOVE ZEROS                  TO WCHAVE-RESTT-LEN  OF GFCTB000.
           MOVE SPACES                 TO WCHAVE-RESTT-TEXT OF GFCTB000.

           EXEC SQL
             SELECT
                   CPROG,
                   CJOB,
                   QREG_COMIT,
                   CSIT_RESTT,
                   CUSUAR_SENHA,
                   HATULZ,
                   WCHAVE_RESTT
             INTO
                   :GFCTB000.CPROG,
                   :GFCTB000.CJOB,
                   :GFCTB000.QREG-COMIT,
                   :GFCTB000.CSIT-RESTT,
                   :GFCTB000.CUSUAR-SENHA
                     :WRK-CUSUAR-SENHA-NULL,
                   :GFCTB000.HATULZ
                     :WRK-HATULZ-NULL,
                   :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL
             FROM   DB2PRD.CTRL_ROTNA_GFCT
             WHERE CPROG              = :GFCTB000.CPROG
              AND  CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               IF  WRK-WCHAVE-RESTT-NULL
                                       NOT EQUAL ZEROS
                   MOVE ZEROS          TO WCHAVE-RESTT-LEN  OF GFCTB000
                   MOVE SPACES         TO WCHAVE-RESTT-TEXT OF GFCTB000
               END-IF
               IF  WRK-CUSUAR-SENHA-NULL
                                       NOT EQUAL ZEROS
                   MOVE SPACES         TO CUSUAR-SENHA      OF GFCTB000
               END-IF
               IF  WRK-HATULZ-NULL     NOT EQUAL ZEROS
                   MOVE '0001-01-01-01.01.01.000001'
                                       TO HATULZ            OF GFCTB000
               END-IF
               MOVE WCHAVE-RESTT-TEXT  OF GFCTB000
                                       TO WRK-REST-CHAVE
               MOVE 'S'                TO WRK-RESTART
           ELSE
               MOVE 'N'                TO WRK-RESTART
           END-IF.

           ADD 1                       TO ACU-LIDOS-GFCTB000.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICAR VAZIO DO ARQUIVO RESTARTI                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-VER-VAZIO-RESTARTI         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2321-LER-RESTARTI.

           IF  WRK-FS-RESTARTI         EQUAL '10'
               PERFORM 2322-MONTAR-DISP-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER O ARQUIVO RESTARTI                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-LER-RESTARTI               SECTION.
      *----------------------------------------------------------------*

           READ RESTARTI.

           IF  WRK-FS-RESTARTI         EQUAL '10'
               GO TO 2321-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-RESTARTI.

           ADD 1                       TO ACU-LIDOS-RESTARTI.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA DISPLAY DE VAZIO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2322-MONTAR-DISP-VAZIO          SECTION.
      *----------------------------------------------------------------*

           MOVE WCHAVE-RESTT-TEXT      TO WRK-REST-CHAVE.
           MOVE QREG-COMIT             OF GFCTB000
                                       TO WRK-MASK-QREG.
           MOVE WRK-REST-AGENCIA       TO WRK-MASK-AGE.
           MOVE WRK-REST-CONTA         TO WRK-MASK-CTA.


           DISPLAY '******************** GFCT1935 *****************'.
           DISPLAY '*                                             *'.
           DISPLAY '* ARQUIVO RESTARTI VAZIO E EXISTE RESTART     *'.
           DISPLAY '*          ERRO GRAVE NO PROCESSAMENTO        *'.
           DISPLAY '*          +++++++++++++++++++++++++++        *'.
           DISPLAY '* QUANTIDADE DE REGISTROS DE RESTART: '
                        WRK-MASK-QREG '   *'.
           DISPLAY '* ULTIMA CHAVE DE PROC:      AGENCIA: '
                        WRK-MASK-AGE '   *'.
           DISPLAY '*                            CONTA..: '
                        WRK-MASK-CTA ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '***********************************************'.

           MOVE WRK-MSG05              TO ERR-TEXTO.

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       2322-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GRAVAR ARQUIVO RESTARTO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-PROCS-RESTARTO             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTU3-REG-RESTART     OF RESTARTI
                                       TO GFCTU3-REG-RESTART
                                       OF RESTARTO.

           PERFORM 2331-GRAVAR-RESTARTO.

           PERFORM 2321-LER-RESTARTI.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *GRAVAR ARQUIVO RESTARTO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2331-GRAVAR-RESTARTO            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCTU3-REG-RESTART    OF RESTARTO.

           PERFORM 1120-TESTAR-FS-RESTARTO.

           ADD 1                       TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2331-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTAR DISPLAY DE FIM DO ARQUIVO RESTARTI ANTES DO NORMAL       *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-MONTAR-DISP-FIM-RESTARTI   SECTION.
      *----------------------------------------------------------------*

           DISPLAY '**************** GFCT1935 *****************'.
           DISPLAY '*                                         *'.
           DISPLAY '* FIM DE RESTARTI ANTES DO FIM DO RESTART *'.
           DISPLAY '*           PROGRAMA CANCELADO            *'.
           DISPLAY '*                                         *'.
           DISPLAY '*******************************************'.

           MOVE WRK-MSG01              TO ERR-TEXTO.

           PERFORM 9999-PROCESSAR-ROTINA-ERRO.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTAR DISPLAY DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2350-MONTAR-DISP-REST           SECTION.
      *----------------------------------------------------------------*

           MOVE QREG-COMIT OF GFCTB000 TO WRK-QREG-COMIT.

           DISPLAY '******************** GFCT1935 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '*                  R E S T A R T                 *'.
           DISPLAY '*                  =============                 *'.
           DISPLAY '*                                                *'.
           DISPLAY '* REGISTROS DE RESTARTI PARA RESTARTO =  '
                      WRK-QREG-COMIT ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.

           MOVE WCHAVE-RESTT-TEXT      OF GFCTB000
                                       TO WRK-REST-CHAVE.
           MOVE WRK-REST-AGENCIA       TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE WRK-REST-CONTA         TO CCTA-DSTNO-MOVTO OF GFCTB092.
           MOVE WRK-REST-LIDOS-TAB     TO ACU-LIDOS-TAB.
           MOVE WRK-REST-ATUALIZADOS   TO ACU-ATUALIZADOS.

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR                                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-ABRIR-CURSOR.

           PERFORM 3200-LER-CURSOR.

           IF  ACU-LIDOS-TAB           EQUAL ZEROS
               DISPLAY '************** GFCT1935 **************'
               DISPLAY '*                                    *'
               DISPLAY '*    NAO EXISTE MOVTO CORRENTISTA    *'
               DISPLAY '*         PARA SER TRAGUEADA         *'
               DISPLAY '*           TERMINO NORMAL           *'
               DISPLAY '*                                    *'
               DISPLAY '*    DATA PROCESSAMENTO: '
                                 DPROCM-ATUAL OF GFCTB0A1 '  *'
               DISPLAY '*                                    *'
               DISPLAY '**************************************'
           END-IF.

           PERFORM 3300-PROCESSAR-CURSOR UNTIL
               WRK-FIM                   EQUAL 'S' OR
               WRK-FS-ARQUTRAG           EQUAL '10'.

           PERFORM 3400-FECHAR-CURSOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM.

           EXEC SQL
               OPEN CSR01-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT  ' TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER CURSOR                                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB092 INTO
                   :GFCTB092.DENVIO-MOVTO-TARIF,
                   :GFCTB092.CROTNA-ORIGE-MOVTO,
                   :GFCTB092.CNRO-ARQ-MOVTO,
                   :GFCTB092.CSEQ-MOVTO,
                   :GFCTB092.CSERVC-TARIF,
                   :GFCTB092.CMIDIA-SERVC-MOVTO,
                   :GFCTB092.CPERIF-SERVC-MOVTO,
                   :GFCTB092.QEVNTO-AGRUP,
                   :GFCTB092.HPREST-SERVC-MOVTO,
                   :GFCTB092.CTPO-CTA-MOVTO,
                   :GFCTB092.CBCO-DSTNO-MOVTO,
                   :GFCTB092.CAG-DSTNO-MOVTO,
                   :GFCTB092.CCTA-DSTNO-MOVTO,
                   :GFCTB092.CPAB-DSTNO-MOVTO,
                   :GFCTB092.CAG-CTLZA-DEB,
                   :GFCTB092.CCTA-CTLZA-DEB,
                   :GFCTB092.VTARIF-BRUTO-MOVTO,
                   :GFCTB092.VTARIF-LIQ-MOVTO,
                   :GFCTB092.VTARIF-DEB-MOVTO,
                   :GFCTB092.DAGNDA-DEB-MOVTO,
                   :GFCTB092.DEFETV-DEB-MOVTO,
                   :GFCTB092.QMAX-TENTV-DEB,
                   :GFCTB092.QTENTV-DEB-MOVTO,
                   :GFCTB092.CSIT-EVNTO-RECBD,
                   :GFCTB092.CIDTFD-PGMC-COBR,
                   :GFCTB092.DOCOR-EVNTO,
                   :GFCTB092.VTARIF-CADTR-SIST,
                   :GFCTB092.CINDCD-TARIF-PCELD
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0               EQUAL 'W'      )
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT  ' TO ERR-DBD-TAB
               MOVE 'FETCH   '        TO ERR-FUN-COMANDO
               MOVE  SQLCODE          TO ERR-SQL-CODE
               MOVE '0040'            TO ERR-LOCAL
               MOVE  SPACES           TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE               EQUAL ZEROS
               MOVE CAG-DSTNO-MOVTO  OF GFCTB092 TO WRK-AGENCIA-BL
               MOVE CCTA-DSTNO-MOVTO OF GFCTB092 TO WRK-CONTA-BL
               ADD 1                 TO ACU-LIDOS-TAB
           ELSE
               MOVE 'S'              TO WRK-FIM
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAR CURSOR                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-BL            GREATER WRK-CHAVE-ARQTRAG
               PERFORM 2100-LER-ARQUTRAG UNTIL
                   WRK-CHAVE-BL        NOT GREATER WRK-CHAVE-ARQTRAG OR
                   WRK-FS-ARQUTRAG     EQUAL '10'
           ELSE
               IF  WRK-CHAVE-BL        LESS WRK-CHAVE-ARQTRAG
                   PERFORM 3200-LER-CURSOR UNTIL
                       WRK-CHAVE-BL    NOT LESS WRK-CHAVE-ARQTRAG OR
                       WRK-FIM         EQUAL 'S'
               ELSE
                   PERFORM 3310-TRATAR-IGUAIS
                   IF  CSIT-EVNTO-RECBD OF GFCTB092 EQUAL 3
                       PERFORM 3320-CARIMBA-ESTORNO
                   END-IF
                   PERFORM 3330-MONTAR-RESTARTO
                   PERFORM 3340-TRATAR-RESTART
                   PERFORM 3200-LER-CURSOR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR UPDATE NA TABELA GFCTB092                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-TRATAR-IGUAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-HAVIA-ESTORNO.
           MOVE GFCTB092               TO WRK-GFCTB092.
           MOVE AGENTRAN               TO CAG-DSTNO-MOVTO  OF GFCTB092.
           MOVE CTANTRAN               TO CCTA-DSTNO-MOVTO OF GFCTB092.

           EXEC SQL
             UPDATE   DB2PRD.MOVTO_EVNTO_CRRTT
               SET
                   CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO,
                   CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'MOVTO_EVNTO_CRRTT  ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-ATUALIZADOS.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR ESTORNO                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-CARIMBA-ESTORNO            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO-CTA-CLI     OF GFCTB0A5.
           MOVE AGEVTRAN               TO CJUNC-DEPDC      OF GFCTB0A5.
           MOVE CTAVTRAN               TO CCTA-CLI         OF GFCTB0A5.
           MOVE WRK-CSERVC-TARIF       TO CSERVC-TARIF     OF GFCTB0A5.
           MOVE 7                      TO CSIT-PROCM-ESTRN OF GFCTB0A5.

           EXEC SQL
             UPDATE   DB2PRD.PDIDO_ESTRN_CTA
               SET
                   CSIT_PROCM_ESTRN   = :GFCTB0A5.CSIT-PROCM-ESTRN
               WHERE
                   CBCO_CTA_CLI     = :GFCTB0A5.CBCO-CTA-CLI
               AND CJUNC_DEPDC      = :GFCTB0A5.CJUNC-DEPDC
               AND CCTA_CLI         = :GFCTB0A5.CCTA-CLI
               AND CSIT_PROCM_ESTRN = 1
               AND CTPO_SOLTC_ESTRN = 1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_ESTRN_CTA ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-HAVIA-ESTORNO
           END-IF.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTAR ARQUIVO RESTARTO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3330-MONTAR-RESTARTO            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                TO GFCTU3-REG-RESTART OF RESTARTO.

           INITIALIZE GFCTU3-REG-RESTART OF RESTARTO.

           MOVE AGEVTRAN             TO GFCTU3-AGENCIA-ANT  OF RESTARTO.
           MOVE CTAVTRAN             TO GFCTU3-CONTA-ANT    OF RESTARTO.
           MOVE WRK-CSERVC-TARIF     TO GFCTU3-TARIFA       OF RESTARTO.
           MOVE WRK-CSIT-EVNTO-RECBD TO GFCTU3-CSIT         OF RESTARTO.
           MOVE WRK-DENVIO-MOVTO-TARIF
                                     TO GFCTU3-DMOVTO-TARIF OF RESTARTO.
           MOVE WRK-CROTNA-ORIGE-MOVTO
                                     TO GFCTU3-CROTNA       OF RESTARTO.
           MOVE WRK-CNRO-ARQ-MOVTO   TO GFCTU3-CNRO-MOVTO   OF RESTARTO.
           MOVE WRK-CSEQ-MOVTO       TO GFCTU3-CSEQ-MOVTO   OF RESTARTO.
           MOVE AGENTRAN             TO GFCTU3-AGENCIA-ATU  OF RESTARTO.
           MOVE CTANTRAN             TO GFCTU3-CONTA-ATU    OF RESTARTO.
           MOVE WRK-HAVIA-ESTORNO   TO GFCTU3-HAVIA-ESTORNO OF RESTARTO.

           PERFORM 2331-GRAVAR-RESTARTO.

      *----------------------------------------------------------------*
       3330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR RESTART                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3340-TRATAR-RESTART             SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-OCORRENCIAS.

           IF  WRK-CHAVE-BL            NOT EQUAL WRK-CHAVE-QUEBRA
               MOVE CAG-DSTNO-MOVTO    OF GFCTB092
                                       TO WRK-AGENCIA-QUEBRA
               MOVE CCTA-DSTNO-MOVTO   OF GFCTB092
                                       TO WRK-CONTA-QUEBRA
               ADD 1                   TO ACU-OCORR-AGENCIAS
               IF  LNK-QTDE-COMMIT         EQUAL ACU-OCORR-AGENCIAS
                   PERFORM 3341-MOVER-CAMPOS
                   IF  WRK-RESTART         EQUAL 'S'
                       PERFORM 3342-UPDATE-GFCTB000
                   ELSE
                       PERFORM 3343-INSERT-GFCTB000
CREMM                  MOVE 'S' TO WRK-RESTART
                   END-IF
                   MOVE ZEROS              TO ACU-OCORR-AGENCIAS
                   PERFORM 3344-EFETUAR-COMMIT
*********          IF  LNK-BRECA           EQUAL 'S'
*********              DISPLAY '************** GFCT1935 **************'
*********              DISPLAY '*                                    *'
*********              DISPLAY '*  CANCELADO APOS O PRIMEIRO COMMIT  *'
*********              DISPLAY '*                                    *'
*********              DISPLAY '************** GFCT1935 **************'
*********              MOVE WRK-MSG02      TO ERR-TEXTO
*********              PERFORM 9999-PROCESSAR-ROTINA-ERRO
*********          END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VALORIZAR AREA PARA RESTART                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3341-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           ADD  ACU-OCORRENCIAS        TO QREG-COMIT        OF GFCTB000.

           MOVE 'GFCT1935'             TO CPROG             OF GFCTB000.
           MOVE WRK-JOBNAME-0160       TO CJOB              OF GFCTB000.
           MOVE AGEVTRAN               TO WRK-REST-AGENCIA.
           MOVE CTAVTRAN               TO WRK-REST-CONTA.
           MOVE ACU-ATUALIZADOS        TO WRK-REST-ATUALIZADOS.
           MOVE ACU-LIDOS-TAB          TO WRK-REST-LIDOS-TAB.
           MOVE WRK-REST-CHAVE         TO WCHAVE-RESTT-TEXT OF GFCTB000.
           MOVE LENGTH                 OF WRK-REST-CHAVE
                                       TO WCHAVE-RESTT-LEN  OF GFCTB000.
           MOVE ZEROS                  TO WRK-WCHAVE-RESTT-NULL.

      *----------------------------------------------------------------*
       3341-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ATUALIZAR TABELA DE RESTART                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3342-UPDATE-GFCTB000            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE DB2PRD.CTRL_ROTNA_GFCT
               SET
                   QREG_COMIT         = :GFCTB000.QREG-COMIT,
                   HATULZ             =  CURRENT TIMESTAMP,
                   WCHAVE_RESTT       = :GFCTB000.WCHAVE-RESTT
                                          :WRK-WCHAVE-RESTT-NULL
             WHERE CPROG              = :GFCTB000.CPROG
              AND  CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'UPDATE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3342-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *INSERIR NA TABELA DE RESTART                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3343-INSERT-GFCTB000            SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO CSIT-RESTT        OF GFCTB000.
           MOVE SPACES                 TO CUSUAR-SENHA      OF GFCTB000.
           MOVE -1                     TO WRK-CUSUAR-SENHA-NULL.

           EXEC SQL
             INSERT INTO DB2PRD.CTRL_ROTNA_GFCT
                  (CPROG,
                   CJOB,
                   QREG_COMIT,
                   CSIT_RESTT,
                   CUSUAR_SENHA,
                   HATULZ,
                   WCHAVE_RESTT)
               VALUES
                  (:GFCTB000.CPROG,
                   :GFCTB000.CJOB,
                   :GFCTB000.QREG-COMIT,
                   :GFCTB000.CSIT-RESTT,
                   :GFCTB000.CUSUAR-SENHA
                     :WRK-CUSUAR-SENHA-NULL,
                   CURRENT TIMESTAMP,
                   :GFCTB000.WCHAVE-RESTT
                     :WRK-WCHAVE-RESTT-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'INSERT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-GRAVADOS-GFCTB000.

      *----------------------------------------------------------------*
       3343-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUAR COMMIT                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3344-EFETUAR-COMMIT             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               COMMIT
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'COMMIT'           TO ERR-DBD-TAB
                                          ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3344-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE  CSR01-GFCTB092
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CTA' TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0100'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZAR PROGRAMA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-ARQUTRAG      NOT EQUAL ZEROS
               PERFORM 4100-DELETE-GFCTB000
           END-IF.

           PERFORM 4200-EMITIR-DISPLAY.

           CLOSE RESTARTI
                 RESTARTO
                 ARQUTRAG.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *DELETAR REGISTRO DE RESTART                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-DELETE-GFCTB000            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1935'             TO CPROG            OF GFCTB000.
           MOVE WRK-JOBNAME-0160       TO CJOB             OF GFCTB000.

           EXEC SQL
             DELETE FROM DB2PRD.CTRL_ROTNA_GFCT
             WHERE
                   CPROG              = :GFCTB000.CPROG    AND
                   CJOB               = :GFCTB000.CJOB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CTRL_ROTNA_GFCT ' TO ERR-DBD-TAB
               MOVE 'DELETE  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0110'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3344-EFETUAR-COMMIT.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EMITIR DISPLAY                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-GFCTB000     TO WRK-MASCARA.

           DISPLAY '******************* GFCT1935 *********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* TOTAL REGS. LIDOS GFCTB000.......: '
                                               WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-ARQUTRAG     TO WRK-MASCARA.

           DISPLAY '* TOTAL REGS. LIDOS ARQUTRAG.......: '
                                               WRK-MASCARA ' *'.

           MOVE ACU-LIDOS-TAB          TO WRK-MASCARA.

           DISPLAY '* TOTAL REGS. LIDOS GFCTB092.......: '
                                               WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS           TO WRK-MASCARA.

           DISPLAY '* TOTAL REGS. GRAVADOS RESTARTO....: '
                                               WRK-MASCARA ' *'.

           MOVE ACU-ATUALIZADOS        TO WRK-MASCARA.

           DISPLAY '* TOTAL REGS. ATUALIZADOS GFCTB092.: '
                                               WRK-MASCARA ' *'.

           MOVE ACU-GRAVADOS-GFCTB000  TO WRK-MASCARA.

           DISPLAY '* TOTAL REGS. GRAVADOS GFCTB000....: '
                                               WRK-MASCARA ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA DE ERRO                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-TIPO-ACESSO         NOT EQUAL 'DB2'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
           END-IF.

           MOVE 'GFCT1935'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
