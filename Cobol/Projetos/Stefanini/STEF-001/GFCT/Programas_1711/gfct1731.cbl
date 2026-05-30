      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*
       PROGRAM-ID. GFCT1731.
       AUTHOR.     RICARDO PEREIRA DA SILVA.
      *===============================================================*
      *                     P R O C W O R K                           *
      *---------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1731                                    *
      *    PROGRAMADOR :  RICARDO PEREIRA DA SILVA - PROCWORK         *
      *    ANALISTA....:  RICARDO PEREIRA DA SILVA - PROCWORK         *
      *    DATA........:  11/06/2008                                  *
      *---------------------------------------------------------------*
      *    OBJETIVO....:  BALANCE-LINE ENTRE OS ARQUIVOS REDGAGCT E   *
      *                   CADASITU.                                   *
      *                   GERAR ARQUIVO CONTENDO CONTAS ATIVAS DO     *
      *                   PERIODO.                                    *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *                DDNAME                      INCLUDE/BOOK       *
      *                   CADASITU                    I#GFCTRV        *
      *                   REDGAGCT                    I#CHEQ04        *
      *                   SELECAO1                    I#GFCT61        *
      *---------------------------------------------------------------*
      *    BOOK'S......:                                              *
      *    I#BRAD7C  - AREA DE COMUNICACAO COM DA POOL7100            *
      *---------------------------------------------------------------*
      *    BCO DE DADOS:                                              *
      *                DB2                                            *
      *                  TABLE                                        *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1         *
      *---------------------------------------------------------------*
      *    MODULOS.....:                                              *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT  REDGAGCT ASSIGN     TO UT-S-REDGAGCT
                      FILE STATUS      IS WRK-FS-REDGAGCT.

           SELECT  CADASITU ASSIGN     TO UT-S-CADASITU
                      FILE STATUS      IS WRK-FS-CADASITU.

           SELECT  SELECAO1 ASSIGN     TO UT-S-SELECAO1
                      FILE STATUS      IS WRK-FS-SELECAO1.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    INPUT:  LAYOUT DE ENTRADA - ARQUIVO DE SITUACAO ATUAL      *
      *            ORG. SEQUENCIAL     -   LRECL   =   060            *
      *---------------------------------------------------------------*

       FD  CADASITU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRV'.

      *---------------------------------------------------------------*
      *    INPUT:  LAYOUT DE ENTRADA - CAD DE CHEQUE REDUZIDO/EQUIVAL.*
      *            ORG. SEQUENCIAL     -   LRECL   =   025            *
      *---------------------------------------------------------------*

       FD  REDGAGCT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#CHEQ04'.

      *---------------------------------------------------------------*
      *    OUTPUT: LAYOUT DE SAIDA - GERACAO MOVTO P/ COBR FICHA CAD. *
      *            ORG. SEQUENCIAL     -   LRECL   =   030            *
      *---------------------------------------------------------------*

       FD  SELECAO1
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING GFCT1731 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CADASITU          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-REDGAGCT          PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SELECAO1       PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZA-CADASITU       PIC  9(012) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZA-REDGAGCT       PIC  9(012) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-CADASITU             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-REDGAGCT             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SELECAO1             PIC  X(002)         VALUE SPACES.

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
           '* AREA PARA MENSAGENS DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.
           05 FILLER                   PIC  X(012)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA BRAD0025'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA            PIC   9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-DIAS             PIC  S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC   9(009) COMP-3  VALUE ZEROS.

       01  WRK-DATA-CORTE-INV          PIC   9(008)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC   9(002)         VALUE ZEROS.
           05  FILLER                  PIC   X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC   9(002)         VALUE ZEROS.
           05  FILLER                  PIC   X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC   9(004)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(009)          VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-DATA1-INV               PIC  9(009)          VALUE ZEROS.
       01  WRK-DATA1-INV-R             REDEFINES WRK-DATA1-INV.
           05  FILLER                  PIC  X(001).
           05  WRK-DATA2-INV           PIC  9(008).

       01  WRK-DATA-0025               PIC  9(009) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA BRAD7100 *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES *'.
      *----------------------------------------------------------------*

       01  WRK-TIPO-TRATO              PIC  9(001)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-TIPO-TRATO.
           05  WRK-TRATO               PIC  X(001).

       01  WRK-DT-ABERTURA             PIC  9(009)         VALUE ZEROS.

       01  WRK-DT-ABER-RED             PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DT-ABER-RED.
           05  FILLER                  PIC  X(001).
           05  WRK-DT-ABER-RED-R       PIC  9(008).

       01  WRK-GFCT-CADASITU.
           05  WRK-GFCT-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  WRK-GFCT-CONTA          PIC  9(007)         VALUE ZEROS.

       01  WRK-CHEQ-REDGAGCT.
           05  WRK-CHEQ-AGENCIA        PIC  9(005)         VALUE ZEROS.
           05  WRK-CHEQ-CONTA          PIC  9(007)         VALUE ZEROS.

       01  WRK-GFCT-CADASITU-ANT.
           05  WRK-GFCT-AG-ANT         PIC  9(005)         VALUE ZEROS.
           05  WRK-GFCT-CTA-ANT        PIC  9(007)         VALUE ZEROS.

       01  WRK-CHEQ-REDGAGCT-ANT.
           05  WRK-CHEQ-AG-ANT         PIC  9(005)         VALUE ZEROS.
           05  WRK-CHEQ-CTA-ANT        PIC  9(007)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '** FIM DA WORKING GFCT1731 **'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

       01  LNK-AREA-PARM.
           05  LNK-TAMA-PARM           PIC S9(004) COMP.
           05  LNK-TIPO-TRATO          PIC  9(001).
           05  LNK-DATA-REF            PIC  9(008).

      *===============================================================*
       PROCEDURE                       DIVISION USING LNK-AREA-PARM.
      *===============================================================*

      *---------------------------------------------------------------*
      *                    INICIO DO PROCESSAMENTO                    *
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2500-VERIFICAR-PARM.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 2600-ACESSAR-DPROCM.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-CHEQ-REDGAGCT  EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *               ROTINA DE INICIALIZACAO                         *
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           OPEN INPUT  CADASITU
                       REDGAGCT
                OUTPUT SELECAO1.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *             ROTINA PARA TESTAR FILE STATUS                    *
      *---------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CADASITU.

           PERFORM 1120-TESTAR-FS-REDGAGCT.

           PERFORM 1130-TESTAR-FS-SELECAO1.

      *---------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          TESTE DE FILE-STATUS DO ARQUIVO CADASITU             *
      *---------------------------------------------------------------*
       1110-TESTAR-FS-CADASITU         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-CADASITU         NOT EQUAL '00'
               MOVE 'CADASITU'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADASITU    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          TESTE DE FILE-STATUS DO ARQUIVO REDGAGCT             *
      *---------------------------------------------------------------*
       1120-TESTAR-FS-REDGAGCT         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-REDGAGCT         NOT EQUAL '00'
               MOVE 'REDGAGCT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-REDGAGCT    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *          TESTE DE FILE-STATUS DO ARQUIVO SELECAO1.            *
      *---------------------------------------------------------------*
       1130-TESTAR-FS-SELECAO1         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SELECAO1         NOT EQUAL '00'
               MOVE 'SELECAO1'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO1    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *       VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS         *
      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *---------------------------------------------------------------*


           PERFORM 2100-LER-CADASITU.

           IF  WRK-GFCT-CADASITU       EQUAL HIGH-VALUES
               DISPLAY '**************** GFCT1731 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*      ARQUIVO CADASITU VAZIO            *'
               DISPLAY '*                                        *'
               DISPLAY '**************** GFCT1731 ****************'
           END-IF.

           PERFORM 2200-LER-REDGAGCT.

           IF  WRK-CHEQ-REDGAGCT       EQUAL HIGH-VALUES
               DISPLAY '**************** GFCT1731 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*      ARQUIVO REDGAGCT VAZIO            *'
               DISPLAY '*                                        *'
               DISPLAY '**************** GFCT1731 ****************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *            LEITURA DO ARQUIVO DE ENTRADA CADASITU             *
      *---------------------------------------------------------------*
       2100-LER-CADASITU               SECTION.
      *---------------------------------------------------------------*

           READ CADASITU.

           IF  WRK-FS-CADASITU         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-GFCT-CADASITU
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CADASITU.

           MOVE GFCTRV-AGENCIA         TO WRK-GFCT-AGENCIA.
           MOVE GFCTRV-CONTA           TO WRK-GFCT-CONTA.

           MOVE GFCTRV-DT-ABERTURA     TO WRK-DT-ABERTURA.
           MOVE WRK-DT-ABERTURA        TO WRK-DT-ABER-RED.

           ADD 1                       TO ACU-LIDOS-CADASITU.

           IF WRK-GFCT-CADASITU-ANT    EQUAL WRK-GFCT-CADASITU
              ADD 1                    TO ACU-DESPREZA-CADASITU
              GO TO 2100-LER-CADASITU
           ELSE
              MOVE WRK-GFCT-CADASITU   TO WRK-GFCT-CADASITU-ANT
           END-IF.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *            LEITURA DO ARQUIVO DE ENTRADA REDGAGCT             *
      *---------------------------------------------------------------*
       2200-LER-REDGAGCT               SECTION.
      *---------------------------------------------------------------*

           READ REDGAGCT.

           IF  WRK-FS-REDGAGCT         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHEQ-REDGAGCT
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-REDGAGCT.

           MOVE CHEQ-AGE               TO WRK-CHEQ-AGENCIA.
           MOVE CHEQ-CTA               TO WRK-CHEQ-CONTA.

           ADD 01                      TO ACU-LIDOS-REDGAGCT.

           IF WRK-CHEQ-REDGAGCT-ANT    EQUAL WRK-CHEQ-REDGAGCT
              ADD 1                    TO ACU-DESPREZA-REDGAGCT
              GO TO 2200-LER-REDGAGCT
           ELSE
              MOVE WRK-CHEQ-REDGAGCT   TO WRK-CHEQ-REDGAGCT-ANT
           END-IF.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2500-VERIFICAR-PARM             SECTION.
      *---------------------------------------------------------------*

           IF  (LNK-TIPO-TRATO         NOT NUMERIC) AND
               (LNK-DATA-REF           NOT NUMERIC)

               DISPLAY '**************** GFCT1731 ****************'
               DISPLAY '*                                        *'
               DISPLAY '*           PARM INVALIDO                *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO           *'
               DISPLAY '*                                        *'
               DISPLAY '**************** GFCT1731 ****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTBA1 E BUSCA DATA PROCM             *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-ACESSAR-DPROCM             SECTION.
      *----------------------------------------------------------------*
           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.
           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO  :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1      TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                   TO WRK-DIA-INV.
           MOVE WRK-MES-DB2                   TO WRK-MES-INV.
           MOVE WRK-ANO-DB2                   TO WRK-ANO-INV.
           MOVE WRK-DATA-INV                  TO WRK-DATA-0025.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE WRK-DATA-0025          TO WRK-DATA-ENTRADA.
           MOVE -180                   TO WRK-NUMERO-DIAS.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'ERRO NA BRAD0025 - DADOS INCONSISTENTES'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-DATA-0025.
           MOVE WRK-DATA-0025          TO WRK-DATA-INV.
           MOVE WRK-DATA-INV           TO WRK-DATA1-INV
           MOVE WRK-DATA2-INV          TO WRK-DATA-CORTE-INV.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
      *                 PROCESSAMENTO PRINCIPAL                       *
      *---------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHEQ-REDGAGCT       EQUAL WRK-GFCT-CADASITU
               IF  LNK-TIPO-TRATO      EQUAL 1 AND
                   WRK-DT-ABER-RED-R   NOT GREATER LNK-DATA-REF

                   MOVE CHEQ-AGE           TO GFCT61-AGENCIA OF SELECAO1
                   MOVE CHEQ-CTA           TO GFCT61-CONTA   OF SELECAO1
                   MOVE CHEQ-CPF-NUM       TO GFCT61-CPF-NUM OF SELECAO1
                   MOVE CHEQ-CPF-CON       TO GFCT61-CPF-CTR OF SELECAO1
                   MOVE WRK-DT-ABER-RED-R  TO GFCT61-DT-ABERTURA-CTA
                                                             OF SELECAO1
                   MOVE LNK-TIPO-TRATO     TO WRK-TIPO-TRATO
                   MOVE WRK-TRATO          TO GFCT61-TIPO-TRATO
                                                             OF SELECAO1

                   MOVE WRK-GRAVACAO       TO WRK-OPERACAO

                   WRITE REG-GFCT61        OF SELECAO1

                   PERFORM 1130-TESTAR-FS-SELECAO1

                   ADD 01                  TO ACU-GRAVADOS-SELECAO1
               ELSE
                   IF  LNK-TIPO-TRATO     EQUAL 2            AND
                       WRK-DT-ABER-RED-R   NOT LESS LNK-DATA-REF AND
                       WRK-DT-ABER-RED-R   LESS WRK-DATA-CORTE-INV

                       MOVE CHEQ-AGE           TO GFCT61-AGENCIA
                                                             OF SELECAO1
                       MOVE CHEQ-CTA           TO GFCT61-CONTA
                                                             OF SELECAO1
                       MOVE CHEQ-CPF-NUM       TO GFCT61-CPF-NUM
                                                             OF SELECAO1
                       MOVE CHEQ-CPF-CON       TO GFCT61-CPF-CTR
                                                             OF SELECAO1
                       MOVE WRK-DT-ABER-RED-R  TO GFCT61-DT-ABERTURA-CTA
                                                             OF SELECAO1
                       MOVE LNK-TIPO-TRATO     TO WRK-TIPO-TRATO
                       MOVE WRK-TRATO          TO GFCT61-TIPO-TRATO
                                                             OF SELECAO1

                       MOVE WRK-GRAVACAO       TO WRK-OPERACAO

                       WRITE REG-GFCT61        OF SELECAO1

                       PERFORM 1130-TESTAR-FS-SELECAO1

                       ADD 01                  TO ACU-GRAVADOS-SELECAO1
                   END-IF
               END-IF

               PERFORM 2200-LER-REDGAGCT
           ELSE
               IF  WRK-CHEQ-REDGAGCT   LESS WRK-GFCT-CADASITU
                   PERFORM 2200-LER-REDGAGCT
               ELSE
                   PERFORM 2100-LER-CADASITU
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *                 FINALIZACAO DE PROCEDIMENTOS                  *
      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           CLOSE REDGAGCT
                 CADASITU
                 SELECAO1.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 4100-EMITIR-DISPLAY.

           STOP RUN.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *  EMITIR ESTATISTICAS DE REGISTROS LIDOS E GRAVADOS.           *
      *---------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *---------------------------------------------------------------*

           DISPLAY '**************** GFCT1731 *********************'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PROGRAMA GFCT1731                 *'.
           DISPLAY '*           -----------------                 *'.
           DISPLAY '*                                             *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO      *'.
           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*---------------------------------------------*'.
           DISPLAY '*                                             *'.
           DISPLAY '* DATA DE CORTE            = 'WRK-DATA-CORTE-INV
                                                        '         *'.
           DISPLAY '*                                             *'.
           MOVE ACU-LIDOS-REDGAGCT     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS REDGAGCT      =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-LIDOS-CADASITU     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS CADASITU      =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-GRAVADOS-SELECAO1  TO WRK-MASCARA.
           DISPLAY '* REGS GRAVADOS SELECAO1   =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-DESPREZA-CADASITU  TO WRK-MASCARA.
           DISPLAY '* REGS DUPLICADOS CADASITU =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           MOVE ACU-DESPREZA-REDGAGCT  TO WRK-MASCARA.
           DISPLAY '* REGS DUPLICADOS REDGAGCT =  ' WRK-MASCARA ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '**************** GFCT1731 *********************'.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *             ROTINA PARA PROCESSAMENTO DE ERRO                 *
      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT1731'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
