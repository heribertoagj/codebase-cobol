      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1800.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                     C P M - S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1800                                     *
      *    PROGRAMADOR.:  ANDRE LUIZ LONGO         -  CPM PATO BRANCO  *
      *    ANALISTA CPM:  FELIPE ALEX SOMMER       -  CPM PATO BRANCO  *
      *    ANALISTA....:  WAGNER SILVA             -  G&P   / GP 50    *
      *    DATA........:  27/08/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  BUSCAR DADOS DA TARIFA E GERAR ARQUIVO PARA  *
      *       APLICACAO DE FRANQUIA E CARENCIA.                        *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ENTFRANQ                 I#GFCTVA           *
      *                    VERFRANQ                 I#GFCTVE           *
      *                    SEMFRANQ                 I#GFCTVA           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                           INCLUDE/BOOK    *
      *                    DB2PRD.PARM_SERVC_TARIF       GFCTB0A2      *
RIC   *                    DB2PRD.MOVTO_EVNTO_CRRTT      GFCTB092      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:  WAGNER SILVA             -  G&P   / GP 50    *
      *    DATA........:  23/10/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  MOVER VALOR CADASTRADO PARA VALOR BRUTO      *
      *    SE TIPO TRATO = 1 OU 51 E VALOR BRUTO ZERADO.               *
      *----------------------------------------------------------------*
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
      *    DATA........:   12/02/2008                                  *
      *    OBJETIVO....:   SUBSTITUICAO DA INC I#GFCTVF PELA I#GFCTVA  *
      *                    ALTERACAO DA INC I#GFCTVE                   *
      *================================================================*
      *
      *===============================================================*
      *                   M A N U T E N C O E S                       *
      *===============================================================*
      *---------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO TAMANHO DA COLUNA DA TABELA   *
      *                    GFCTBA2 - QDIA_CAREN_TARIF DE 9(02) PARA   *
      *                    9(03) - JC0413                             *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   J.C.R.                    - SONDA IT       *
      *    ANALISTA DDS:   HELENA                    - GP.50          *
      *    ANALISTA....:   PAGNNOCA                  - SONDA IT       *
      *    DATA........:   ABRIL/2013                                 *
      *---------------------------------------------------------------*
      *===============================================================*
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

           SELECT ENTFRANQ  ASSIGN     TO UT-S-ENTFRANQ
           FILE STATUS                 IS WRK-FS-ENTFRANQ.

           SELECT VERFRANQ  ASSIGN     TO UT-S-VERFRANQ
           FILE STATUS                 IS WRK-FS-VERFRANQ.

           SELECT SEMFRANQ  ASSIGN     TO UT-S-SEMFRANQ
           FILE STATUS                 IS WRK-FS-SEMFRANQ.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE DADOS DAS TABELAS GFCTB033 E GFCTB092    *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  ENTFRANQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO PARA FRANQUIA  E CARENCIA                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  VERFRANQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-VERFRANQ                PIC  X(300).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE REG. SEM FRANQUIA P/CONTINUAR PROC. CO-  *
      *            BRANCA                                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SEMFRANQ
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-SEMFRANQ                 PIC  X(300).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS                   PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-VERFRANQ       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SEMFRANQ       PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-NAO-ACHOU               PIC  X(001)         VALUE SPACES.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-INV             PIC  9(002)         VALUE ZEROS.
           05  WRK-DIA-INV             PIC  9(002)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV
                                       PIC  9(008).

       01  WRK-CHV-ENTRADA.
           05  WRK-CHV-ENT-TARIFA      PIC  9(005)         VALUE ZEROS.
           05  WRK-CHV-ENT-DT-OCOR     PIC  9(008)         VALUE ZEROS.

       01  WRK-CHV-PERIODO.
           05  WRK-CHV-PER-TARIFA      PIC  9(005)         VALUE ZEROS.
           05  WRK-CHV-PER-DT-INI      PIC  9(008)         VALUE ZEROS.
           05  WRK-CHV-PER-DT-FIM      PIC  9(008)         VALUE ZEROS.

       01  WRK-GFCTB0A2.
           05  WRK-QFRANQ-TARIF        PIC S9(003) COMP-3  VALUE ZEROS.
           05  WRK-CTPO-FRANQ-TARIF    PIC S9(001) COMP-3  VALUE ZEROS.
           05  WRK-CPER-FRANQ-TARIF    PIC S9(001) COMP-3  VALUE ZEROS.
           05  WRK-CPER-CALC-TARIF     PIC S9(001) COMP-3  VALUE ZEROS.
           05  WRK-CINIC-PER-CALC      PIC S9(001) COMP-3  VALUE ZEROS.
           05  WRK-DDIA-COBR-TARIF     PIC S9(002) COMP-3  VALUE ZEROS.
JC0413*****05**WRK-QDIA-CAREN-TARIF****PIC*S9(002)*COMP-3**VALUE*ZEROS.*
JC0413     05  WRK-QDIA-CAREN-TARIF    PIC S9(003) COMP-3  VALUE ZEROS.
           05  WRK-PDESC-CAREN-TARIF   PIC S9(003)V9999 COMP-3
                                                           VALUE ZEROS.
           05  WRK-CINDCD-INIC-CAREN   PIC S9(001) COMP-3  VALUE ZEROS.
           05  WRK-QDIA-RTCAO-COBR     PIC S9(002) COMP-3  VALUE ZEROS.
           05  WRK-DINIC-CAREN-TARIF   PIC  X(010)         VALUE SPACES.
           05  WRK-CSEQ-MANUT-SERVC    PIC S9(002) COMP-3  VALUE ZEROS.
           05  WRK-CINIC-PER-FRANQ     PIC S9(001) COMP-3  VALUE ZEROS.

RIC    77  WRK-REFT-EXTRT          PIC S9(004) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTFRANQ             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-VERFRANQ             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SEMFRANQ             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGEM DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ARQUIVOS DE SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A2
           END-EXEC.

RIC        EXEC SQL
RIC            INCLUDE GFCTB092
RIC        END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *INICIO DO PROGRAMA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ENTFRANQ     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABERTURA DE ARQUIVOS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ENTFRANQ
                OUTPUT VERFRANQ
                       SEMFRANQ.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTES DE FILE STATUS                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ENTFRANQ.

           PERFORM 1120-TESTAR-FS-VERFRANQ.

           PERFORM 1130-TESTAR-FS-SEMFRANQ.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTE DE FILE STATUS DO ARQUIVO DE ENTRADA                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ENTFRANQ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTFRANQ         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTFRANQ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTFRANQ    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTE DE FILE STATUS DO ARQUIVO DE SAIDA                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-VERFRANQ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-VERFRANQ         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'VERFRANQ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-VERFRANQ    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TESTE DE FILE STATUS DO ARQUIVO DE SAIDA                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-SEMFRANQ         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEMFRANQ         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SEMFRANQ'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SEMFRANQ    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICACAO DE ARQUIVO VAZIO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ENTFRANQ.

           IF  WRK-FS-ENTFRANQ         EQUAL '10'
               DISPLAY '************* GFCT1800 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO DE ENTFRANQ VAZIO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT1800 *************'
               MOVE 0004               TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LEITURA DO ARQUIVO DE ENTRADA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ENTFRANQ               SECTION.
      *----------------------------------------------------------------*

           READ ENTFRANQ.

           IF  WRK-FS-ENTFRANQ             EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA                TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ENTFRANQ.

           MOVE GFCTVA-CSERVC-TARIF        OF ENTFRANQ
                                           TO WRK-CHV-ENT-TARIFA.
           MOVE GFCTVA-DOCOR-EVNTO         OF ENTFRANQ
                                           TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX                TO WRK-DIA-INV.
           MOVE WRK-MES-AUX                TO WRK-MES-INV.
           MOVE WRK-ANO-AUX                TO WRK-ANO-INV.
           MOVE WRK-DATA-INV-R             TO WRK-CHV-ENT-DT-OCOR.

           ADD 1                           TO ACU-LIDOS.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL DO PROGRAMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-NAO-ACHOU.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  WRK-CHV-ENT-TARIFA      NOT EQUAL WRK-CHV-PER-TARIFA OR
               WRK-CHV-ENT-DT-OCOR     LESS WRK-CHV-PER-DT-INI OR
               WRK-CHV-ENT-DT-OCOR     GREATER WRK-CHV-PER-DT-FIM
               PERFORM 3100-BUSCAR-TARIFA
           END-IF.

           IF  WRK-NAO-ACHOU           NOT EQUAL 'S'
               PERFORM 3300-GRAVAR-SAIDA
           END-IF.

           PERFORM 2100-LER-ENTFRANQ.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *BUSCAR DADOS DA TARIFA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-BUSCAR-TARIFA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSERVC-TARIF    OF ENTFRANQ
                                       TO CSERVC-TARIF      OF GFCTB0A2.
           MOVE GFCTVA-DOCOR-EVNTO     OF ENTFRANQ
                                       TO DINIC-VGCIA-TARIF OF GFCTB0A2
                                          DFIM-VGCIA-TARIF  OF GFCTB0A2.

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
                   QFRANQ_TARIF,
                   CPER_FRANQ_TARIF,
                   CPER_CALC_TARIF,
                   CINIC_PER_CALC,
                   DDIA_COBR_TARIF,
                   QDIA_CAREN_TARIF,
                   PDESC_CAREN_TARIF,
                   CINDCD_INIC_CAREN,
                   QDIA_RTCAO_COBR,
                   DINIC_CAREN_TARIF,
                   CSEQ_MANUT_SERVC,
                   CINIC_PER_FRANQ,
                   CTPO_FRANQ_TARIF
             INTO
                   :GFCTB0A2.CSERVC-TARIF,
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF,
                   :GFCTB0A2.QFRANQ-TARIF,
                   :GFCTB0A2.CPER-FRANQ-TARIF,
                   :GFCTB0A2.CPER-CALC-TARIF,
                   :GFCTB0A2.CINIC-PER-CALC,
                   :GFCTB0A2.DDIA-COBR-TARIF,
JC0413             :GFCTB0A2.QDIA-CAREN-TARIF,
                   :GFCTB0A2.PDESC-CAREN-TARIF,
                   :GFCTB0A2.CINDCD-INIC-CAREN,
                   :GFCTB0A2.QDIA-RTCAO-COBR,
                   :GFCTB0A2.DINIC-CAREN-TARIF,
                   :GFCTB0A2.CSEQ-MANUT-SERVC,
                   :GFCTB0A2.CINIC-PER-FRANQ,
                   :GFCTB0A2.CTPO-FRANQ-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF       AND
                   DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF  AND
                   DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
ATEND           ORDER  BY HIDTFD_MANUT_SERVC DESC
ATEND           FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF'    TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0010'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               ADD 1                   TO ACU-DESPREZADOS
               MOVE 'S'                TO WRK-NAO-ACHOU
           ELSE
               MOVE CSERVC-TARIF       OF GFCTB0A2
                                       TO WRK-CHV-PER-TARIFA
               MOVE DINIC-VGCIA-TARIF  OF GFCTB0A2
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO WRK-DIA-INV
               MOVE WRK-MES-AUX        TO WRK-MES-INV
               MOVE WRK-ANO-AUX        TO WRK-ANO-INV
               MOVE WRK-DATA-INV-R     TO WRK-CHV-PER-DT-INI
               MOVE DFIM-VGCIA-TARIF   OF GFCTB0A2
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        TO WRK-DIA-INV
               MOVE WRK-MES-AUX        TO WRK-MES-INV
               MOVE WRK-ANO-AUX        TO WRK-ANO-INV
               MOVE WRK-DATA-INV-R     TO WRK-CHV-PER-DT-FIM
               MOVE QFRANQ-TARIF       OF GFCTB0A2
                                       TO WRK-QFRANQ-TARIF
               MOVE CTPO-FRANQ-TARIF   OF GFCTB0A2
                                       TO WRK-CTPO-FRANQ-TARIF
               MOVE CPER-FRANQ-TARIF   OF GFCTB0A2
                                       TO WRK-CPER-FRANQ-TARIF
               MOVE CPER-CALC-TARIF    OF GFCTB0A2
                                       TO WRK-CPER-CALC-TARIF
               MOVE CINIC-PER-CALC     OF GFCTB0A2
                                       TO WRK-CINIC-PER-CALC
               MOVE DDIA-COBR-TARIF    OF GFCTB0A2
                                       TO WRK-DDIA-COBR-TARIF
JC0413         MOVE QDIA-CAREN-TARIF   OF GFCTB0A2
JC0413                                 TO WRK-QDIA-CAREN-TARIF
               MOVE PDESC-CAREN-TARIF  OF GFCTB0A2
                                       TO WRK-PDESC-CAREN-TARIF
               MOVE CINDCD-INIC-CAREN  OF GFCTB0A2
                                       TO WRK-CINDCD-INIC-CAREN
               MOVE QDIA-RTCAO-COBR    OF GFCTB0A2
                                       TO WRK-QDIA-RTCAO-COBR
               MOVE DINIC-CAREN-TARIF  OF GFCTB0A2
                                       TO WRK-DINIC-CAREN-TARIF
               MOVE CSEQ-MANUT-SERVC   OF GFCTB0A2
                                       TO WRK-CSEQ-MANUT-SERVC
               MOVE CINIC-PER-FRANQ    OF GFCTB0A2
                                       TO WRK-CINIC-PER-FRANQ
           END-IF.


      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

RIC   ******************************************************************
RIC   *BUSCAR ANO DE REFERENCIA                                        *
RIC   ******************************************************************
RIC   *----------------------------------------------------------------*
RIC    3200-BUSCAR-ANO                 SECTION.
RIC   *----------------------------------------------------------------*

RIC        MOVE GFCTVA-DENVIO-MOVTO-TARIF OF ENTFRANQ
RIC                                 TO DENVIO-MOVTO-TARIF OF GFCTB092.
RIC        MOVE GFCTVA-CROTNA-ORIGE-MOVTO OF ENTFRANQ
RIC                                 TO CROTNA-ORIGE-MOVTO OF GFCTB092.
RIC        MOVE GFCTVA-CNRO-ARQ-MOVTO     OF ENTFRANQ
RIC                                 TO CNRO-ARQ-MOVTO     OF GFCTB092.
RIC        MOVE GFCTVA-CSEQ-MOVTO         OF ENTFRANQ
RIC                                 TO CSEQ-MOVTO         OF GFCTB092.
RIC        EXEC SQL
RIC          SELECT
RIC                DANO_REFT_EXTRT
RIC          INTO
RIC                :GFCTB092.DANO-REFT-EXTRT
RIC          FROM   DB2PRD.MOVTO_EVNTO_CRRTT
RIC          WHERE
RIC               CROTNA_ORIGE_MOVTO  = :GFCTB092.CROTNA-ORIGE-MOVTO AND
RIC               DENVIO_MOVTO_TARIF  = :GFCTB092.DENVIO-MOVTO-TARIF AND
RIC               CNRO_ARQ_MOVTO      = :GFCTB092.CNRO-ARQ-MOVTO     AND
RIC               CSEQ_MOVTO          = :GFCTB092.CSEQ-MOVTO
RIC        END-EXEC.

RIC        IF  (SQLCODE                    NOT EQUAL ZEROS ) OR
RIC            (SQLWARN0                   EQUAL 'W')
RIC             MOVE 'DB2'                 TO ERR-TIPO-ACESSO
RIC             MOVE 'MOVTO_EVNTO_CRRTT'   TO ERR-DBD-TAB
RIC             MOVE 'SELECT    '          TO ERR-FUN-COMANDO
RIC             MOVE SQLCODE               TO ERR-SQL-CODE
RIC             MOVE '0020'                TO ERR-LOCAL
RIC             MOVE SPACES                TO ERR-SEGM
RIC             PERFORM 9999-PROCESSAR-ROTINA-ERRO
RIC        END-IF.

RIC        MOVE DANO-REFT-EXTRT            OF GFCTB092
RIC                                        TO WRK-REFT-EXTRT.

RIC   *----------------------------------------------------------------*
RIC    3200-99-FIM.                    EXIT.
RIC   *----------------------------------------------------------------*

      ******************************************************************
      *GRAVACAO DO ARQUIVO DE SAIDA VERFRANQ                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

RIC        IF WRK-CPER-FRANQ-TARIF EQUAL 5
RIC           PERFORM 3200-BUSCAR-ANO
RIC        END-IF.

           MOVE SPACES                 TO REG-IGFCTVE.

           INITIALIZE REG-IGFCTVE.

           MOVE GFCTVA-GFCTB092        OF ENTFRANQ
                                       TO VE-GFCTB092.
           MOVE GFCTVA-CTRATO-MOVTO    TO VE-CTRATO-MOVTO.
RIC        MOVE WRK-REFT-EXTRT         TO VE-DANO-REFT-EXTRT.
           MOVE WRK-GFCTB0A2           TO VE-GFCTB0A2.

           IF   VE-CTRATO-MOVTO         = 01       OR
                VE-CTRATO-MOVTO         = 51
                IF VE-VTARIF-BRUTO-MOVTO  = ZEROS AND
                   VE-VTARIF-CADTR-SIST   > ZEROS
                   MOVE VE-VTARIF-CADTR-SIST
                     TO VE-VTARIF-BRUTO-MOVTO VE-VTARIF-LIQ-MOVTO
                END-IF
           END-IF.

           IF   QFRANQ-TARIF           OF GFCTB0A2
                                    EQUAL ZEROS     OR
                CTPO-FRANQ-TARIF        OF GFCTB0A2
                                    NOT EQUAL ZEROS
                WRITE FD-SEMFRANQ   FROM REG-IGFCTVE
                PERFORM 1130-TESTAR-FS-SEMFRANQ
                ADD 1                  TO ACU-GRAVADOS-SEMFRANQ
            ELSE
                WRITE REG-VERFRANQ   FROM REG-IGFCTVE
                PERFORM 1120-TESTAR-FS-VERFRANQ
                ADD 1                  TO ACU-GRAVADOS-VERFRANQ
RICX            MOVE ZEROS             TO WRK-REFT-EXTRT
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAMENTO DOS ARQUIVOS E FINALIZACAO DO PROGRAMA               *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS.

           CLOSE ENTFRANQ
                 VERFRANQ
                 SEMFRANQ.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EMISSAO DE TOTAIS DE CONTROLE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS              TO WRK-MASCARA.

           DISPLAY '****************************** GFCT1800 ***********'
                                                        '*************'.
           DISPLAY '*                                                '
                                                      '              *'.
           DISPLAY '*  TOTAL REGS. LIDOS ENTFRANQ            =   '
                                              ' ' WRK-MASCARA '      *'.

           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '*  TOTAL REGS. DESPREZADOS               =   '
                                              ' ' WRK-MASCARA '      *'.

           MOVE ACU-GRAVADOS-SEMFRANQ   TO WRK-MASCARA.

           DISPLAY '*  TOTAL REGS. GRAVADOS SEMFRANQ         =   '
                                              ' ' WRK-MASCARA '      *'.

           MOVE ACU-GRAVADOS-VERFRANQ   TO WRK-MASCARA.

           DISPLAY '*  TOTAL REGS. GRAVADOS VERFRANQ         =   '
                                              ' ' WRK-MASCARA '      *'.

           DISPLAY '*                                                '
                                                      '              *'.
           DISPLAY '****************************** GFCT1800 ***********'
                                                        '*************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA DE ERRO                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1800'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
