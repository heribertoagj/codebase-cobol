      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID. GFCT2090.
       AUTHOR. MCRISTINA.
      *===============================================================*
      *                   SONDA - PROCWORK                            *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMA     : GFCT2090                                  *
      *      PROGRAMADOR  : MCRISTINA        - SONDA/PROCWORK         *
      *      ANALISTA     : MCRISTINA        - SONDA/PROCWORK         *
      *      DATA         : MARCO/2008                                *
      *                                                               *
      *      OBJETIVO     :                                           *
      *        SEPARA REGISTROS PARA O PROCESSO DE IDENTIFICACAO      *
      *        DE SEGMENTOS PARA CONTAS NOVAS .                       *
      *                                                               *
      *      ARQUIVOS:                                                *
      *      ---------                                                *
      *      DDNAME                              INCLUDE/BOOK         *
      *      CADNGERA                              I#CLIE20           *
      *      CADANOVO                              I#CLIE20           *
      *                                                               *
      *      MODULOS CHAMADOS:                                        *
      *      -----------------                                        *
      *      BRAD0300 - LEITURA DE ARQUIVO COMPRIMIDO.                *
      *      BRAD7100 - TRATAMENTO DE ERROS.                          *
      *      BRAD1050 - FAZ CONEXAO COM DB2.                          *
      *                                                               *
      *      TABELAS :                                                *
      *      ---------                                                *
      *      DB2PRD.PARM_DATA_PROCM        GFCTB0A1                   *
      *                                                               *
      *===============================================================*
      *                      A L T E R A C A O                        *
      *---------------------------------------------------------------*
      *      ANALISTA     : WAGNER SILVA     - SONDA/PROCWORK         *
      *      DATA         : 16/04/2008                                *
      *      OBJETIVO     : SUBSTITUIR DPROCM-ANTER POR DPROCM-ATUAL  *
      *                     PARA CALCULO DA DATA DE INICIO.           *
      *===============================================================*
      *===============================================================*
      *                      A L T E R A C A O                        *
      *---------------------------------------------------------------*
      *      ANALISTA     : WAGNER SILVA     - SONDA/PROCWORK         *
      *      DATA         : 17/10/2008                                *
      *      OBJETIVO     : EXCLUIR ULTIMO BYTE DO SUBGRUPO DA RAZAO  *
      *===============================================================*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

           EJECT
      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT CADNGERA ASSIGN      TO UT-S-CADNGERA
           FILE STATUS                 IS WRK-FS-CADNGERA.

           SELECT CADANOVO  ASSIGN     TO UT-S-CADANOVO
           FILE STATUS                 IS WRK-FS-CADANOVO.

           EJECT
      *===============================================================*
       DATA DIVISION.
      *===============================================================*

       FILE SECTION.

      *---------------------------------------------------------------*
      *    INPUT:  CADASTRO DE CLIENTES - COMPRIMIDO.                 *
      *            ORG. SEQUENCIAL     -   LRECL   =  180             *
      *---------------------------------------------------------------*

        FD  CADNGERA
            RECORDING MODE IS F
            LABEL RECORD IS STANDARD
            BLOCK CONTAINS 0 RECORDS.

        01  REG-CADNGERA                 PIC X(180).

      *---------------------------------------------------------------*
      *    OUTPUT:    CADASTRO DE CLIENTES - DESCOMPRIMIDO.           *
      *               ORG. SEQUENCIAL   -   LRECL = 180               *
      *---------------------------------------------------------------*

       FD  CADANOVO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-CADANOVO                   PIC X(180).

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER PIC X(32) VALUE  '*  INICIO DA WORKING GFCT2090  *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(029)         VALUE
           'AREA PARA FORMATACAO DE ERROS'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05  FILLER                  PIC  X(008)        VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)        VALUE SPACES.
           05  FILLER                  PIC  X(012)        VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)        VALUE SPACES.
           05  FILLER                  PIC  X(017)        VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)        VALUE SPACES.
           05  FILLER                  PIC  X(015)        VALUE
               ' **'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *---------------------------------------------------------------*
       77  FILLER PIC X(32) VALUE  '*         ACUMULADORES         *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-CADNGERA          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVADOS                PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC 9(09) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER PIC X(32) VALUE  '*  AREAS P/TESTE FILE STATUS   *'.
      *---------------------------------------------------------------*

       77  WRK-FS-CADNGERA            PIC X(02)        VALUE  SPACES.
       77  WRK-FS-CADANOVO            PIC X(02)        VALUE  SPACES.

       77  WRK-ABERTURA               PIC X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                PIC X(13) VALUE  'NA LEITURA'.
       77  WRK-GRAVACAO               PIC X(13) VALUE  'NA GRAVACAO'.
       77  WRK-FECHAMENTO             PIC X(13) VALUE  'NO FECHAMENTO'.
       77  WRK-ABEND                  PIC S9(04) COMP  VALUE +1111.

      *---------------------------------------------------------------*
       77  FILLER PIC X(32) VALUE  '*       AREAS AUXILIARES       *'.
      *---------------------------------------------------------------*

       77  WRK-CADNGERA                PIC X(08) VALUE 'CADNGERA'.
       77  WRK-CADANOVO                PIC X(08) VALUE 'CADANOVO'.
       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH'.
       77  WRK-EDITADO                 PIC ZZZ.ZZZ.ZZ9.

       01  WRK-CAD-AGENCIA.
           03 WRK-AGENCIA              PIC 9(05) VALUE ZEROS.

       01  WRK-RAZAO.
           03 WRK-GRUPO-RAZAO          PIC 9(02) VALUE ZEROS.
           03 WRK-SUBGRUPO-RAZAO       PIC 9(02) VALUE ZEROS.
1008       03 WRK-SUBGRUPO-RESTO       PIC 9(01) VALUE ZEROS.
       01  WRK-RAZAO-R REDEFINES WRK-RAZAO PIC 9(05).

       01  WRK-DT-ABERT-ANT.
           03  WRK-DIA-A               PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE '.'.
           03  WRK-MES-A               PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE '.'.
           03  WRK-ANO-A               PIC  9(004)      VALUE ZEROS.

       01  WRK-DT-ABERT-OK             PIC  9(009)      VALUE ZEROS.
       01  WRK-DT-ABERT-OK-R           REDEFINES WRK-DT-ABERT-OK.
           03 FILLER                   PIC 9(001).
           03 WRK-ANO-O                PIC 9(004).
           03 WRK-MES-O                PIC 9(002).
           03 WRK-DIA-O                PIC 9(002).

       01  WRK-DT-ABERT-CAD.
           03 FIILER                   PIC 9(001) VALUE ZEROS.
           03 WRK-ANO-C                PIC 9(004) VALUE ZEROS.
           03 WRK-MES-C                PIC 9(002) VALUE ZEROS.
           03 WRK-DIA-C                PIC 9(002) VALUE ZEROS.
       01  WRK-DT-ABERT-CAD-R REDEFINES WRK-DT-ABERT-CAD PIC 9(009).

       01  WRK-DT-DB2.
           03  WRK-DIA-DB2             PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)      VALUE ZEROS.
           03  FILLER                  PIC  X(001)      VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)      VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA BRAD0025'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA            PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-DIAS             PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC  9(009) COMP-3  VALUE ZEROS.

       01  WRK-DATA                    PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA.
           05  FILLER                  PIC  9(001).
           05  WRK-DATA-PARM-8         PIC  X(008).

       01  WRK-DATA-PARM               PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-PARM-R             REDEFINES WRK-DATA-PARM.
           05 FILLER                   PIC  X(001).
           05 WRK-DIA-PARM             PIC  9(002).
           05 WRK-MES-PARM             PIC  9(002).
           05 WRK-ANO-PARM             PIC  9(004).

       01  WRK-MSG01                   PIC  X(040) VALUE
           'ERRO NO ACESSO A BRAD0025'.

       01  WRK-MSG02                   PIC  X(040) VALUE
           'DATA INCONSISTENTE NO PARM'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS PARA OBTER DATA E HORA DO SISTEMA'.

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC  9(005) COMP-3 VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC  9(007) COMP-3 VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC  9(007) COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3 VALUE ZEROS.
           05  WRK-TIMESTAMP           PIC  X(020)        VALUE SPACES.

       01  WRK-AAAAMMDD                PIC  9(009) VALUE ZEROS.
       01  WRK-AAAAMMDD-R REDEFINES    WRK-AAAAMMDD.
           05  FILLER                  PIC  9(001).
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-DATA-P                  PIC  9(009) VALUE ZEROS.
       01  WRK-DATA-P-R REDEFINES      WRK-DATA-P.
           05  FILLER                  PIC  9(001).
           05  WRK-ANO-P               PIC  9(004).
           05  WRK-MES-P               PIC  9(002).
           05  WRK-DIA-P               PIC  9(002).

       01  WRK-DATA-FIM                PIC  9(009) VALUE ZEROS.

      *---------------------------------------------------------------*
      *                 AREA PARA VARIAVEIS DA BRAD0300               *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA I#CLIE20'.
      *---------------------------------------------------------------*

        COPY 'I#CLIE20'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA I#BRAD7C'.
      *---------------------------------------------------------------*

        COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01  FILLER PIC X(32) VALUE  '*  INICIO DA WORKING GFCT2090  *'.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-AREA-PARM.
          05  LNK-TAMA-PARM           PIC S9(004) COMP.
          05  LNK-DATA-PARM           PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION USING LNK-AREA-PARM.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           IF LNK-DATA-PARM IS NUMERIC OR
              LNK-DATA-PARM EQUAL ZEROS
              NEXT SENTENCE
           ELSE
              DISPLAY '*********** GFCT2090 ***********'
              DISPLAY '*                              *'
              DISPLAY '*  PARM DIFERENTE DE DATA OU   *'
              DISPLAY '*            ZEROS             *'
              DISPLAY '*                              *'
              DISPLAY '*********** GFCT2090 ***********'
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 2049-OBTER-DATA-PROCESSAMENTO.

           PERFORM 2052-ULTIMO-DIA-MES-ANTERIOR.

           IF LNK-DATA-PARM EQUAL ZEROS
               PERFORM 2050-OBTER-DATA-INICIO
           ELSE
               PERFORM 2051-TRATAR-DATA-PARM
           END-IF.

           DISPLAY '** DPROCM =' DPROCM-ATUAL OF GFCTB0A1
                   '  DT.INICIO =' WRK-DT-ABERT-OK ' **'
                   '  DT.FIM =' WRK-DATA-FIM ' **'.

           PERFORM 3000-PROCESSO       UNTIL
                   WRK-CAD-AGENCIA     EQUAL     HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT   CADNGERA
                OUTPUT  CADANOVO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CADNGERA.
           PERFORM 1130-TESTAR-FS-CADANOVO.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1120-TESTAR-FS-CADNGERA         SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADNGERA          NOT EQUAL '00'
              DISPLAY '************** GFCT2090 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADNGERA             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADNGERA
                                                 '         *'
              DISPLAY '************** GFCT2090 *************'
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1130-TESTAR-FS-CADANOVO         SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADANOVO          NOT EQUAL '00'
              DISPLAY '************** GFCT2090 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              CADANOVO             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADANOVO
                                                 '         *'
              DISPLAY '************** GFCT2090 *************'
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-CADNGERA.

           IF WRK-CAD-AGENCIA          EQUAL HIGH-VALUES
              DISPLAY '*********** GFCT2090 ***********'
              DISPLAY '*                              *'
              DISPLAY '*    ARQUIVO CADNGERA VAZIO    *'
              DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
              DISPLAY '*                              *'
              DISPLAY '*********** GFCT2090 ***********'
              PERFORM 4000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      * OBTER DATA DO PROCESSAMENTO
      *****************************************************************
      *---------------------------------------------------------------*
       2049-OBTER-DATA-PROCESSAMENTO   SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT'                     TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
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

      *---------------------------------------------------------------*
       2049-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2050-OBTER-DATA-INICIO   SECTION.
      *---------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1  TO WRK-DT-ABERT-ANT.

           MOVE 01                        TO WRK-DIA-A.

           IF  WRK-MES-A                 EQUAL 01
               MOVE                      12 TO WRK-MES-A
               COMPUTE WRK-ANO-A = WRK-ANO-A - 1
           ELSE
               COMPUTE WRK-MES-A = WRK-MES-A - 1
           END-IF.

           MOVE WRK-ANO-A                 TO WRK-ANO-O
           MOVE WRK-MES-A                 TO WRK-MES-O
           MOVE WRK-DIA-A                 TO WRK-DIA-O.

      *---------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2051-TRATAR-DATA-PARM   SECTION.
      *---------------------------------------------------------------*

           MOVE 0      TO WRK-NUMERO-DIAS.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE LNK-DATA-PARM          TO WRK-DATA-PARM-8
           MOVE WRK-DATA               TO WRK-DATA-ENTRADA.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MSG02          TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF WRK-DATA GREATER WRK-DATA-FIM
              DISPLAY '*********** GFCT2090 ***********'
              DISPLAY '*                              *'
              DISPLAY '*      DATA PARM MAIOR QUE     *'
              DISPLAY '*           DATA FIM           *'
              DISPLAY '*                              *'
              DISPLAY '* DATA PARM = 'WRK-DATA
                                             '        *'
              DISPLAY '* DATA FIM  = 'WRK-DATA-FIM
                                             '        *'
              DISPLAY '*                              *'
              DISPLAY '*********** GFCT2090 ***********'
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA                  TO WRK-DATA-P
           MOVE WRK-ANO-P                 TO WRK-ANO-O
           MOVE WRK-MES-P                 TO WRK-MES-O
           MOVE WRK-DIA-P                 TO WRK-DIA-O.

      *---------------------------------------------------------------*
       2051-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2052-ULTIMO-DIA-MES-ANTERIOR   SECTION.
      *---------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1 TO WRK-DT-DB2.
           MOVE WRK-MES-DB2              TO WRK-MES-INV
           MOVE WRK-ANO-DB2              TO WRK-ANO-INV
           MOVE 01                       TO WRK-DIA-INV.

           MOVE -1                     TO WRK-NUMERO-DIAS.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE WRK-AAAAMMDD           TO WRK-DATA-ENTRADA.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MSG01          TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-DATA-FIM.

      *---------------------------------------------------------------*
       2052-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-LER-CADNGERA               SECTION.
      *---------------------------------------------------------------*

           CALL 'BRAD0300'             USING WRK-CADNGERA
                                             REG-CADACLIE.

           IF RETURN-CODE              EQUAL 4
              MOVE HIGH-VALUES         TO WRK-CAD-AGENCIA
              GO                       TO 2100-99-FIM
           END-IF.

           ADD 1                          TO ACU-LIDOS-CADNGERA.

           MOVE CAD-AGENCIA               TO WRK-AGENCIA.

      *---------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       3000-PROCESSO                   SECTION.
      *---------------------------------------------------------------*

           MOVE CAD-RAZAO                 TO WRK-RAZAO-R.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  * HX_IF (CAD-AGENCIA            GREATER 1)     AND
HEXA  *       (CAD-AGENCIA            LESS    4000)  AND
HEXA  *       (WRK-GRUPO-RAZAO        EQUAL   7)     AND
HEXA  *       (WRK-SUBGRUPO-RAZAO     NOT EQUAL 9 AND 13 AND 38 )  AND
HEXA  *       (WRK-SUBGRUPO-RAZAO     NOT EQUAL  50 AND 78 )       AND
HEXA  *       (CAD-DT-ABERT           NOT LESS WRK-DT-ABERT-OK)    AND
HEXA  *       (CAD-DT-ABERT           NOT GREATER WRK-DATA-FIM)    AND
HEXA  *       (CAD-IDENTF-CTA         NOT EQUAL '1' AND '01')      AND
HEXA  *       (CAD-IDENTF-CTA         NOT EQUAL ' 1' AND '1 ')     AND
HEXA  *       (CAD-IDENTF-CTA         NOT EQUAL '4' AND '04')      AND
HEXA  *       (CAD-IDENTF-CTA         NOT EQUAL ' 4' AND '4 ')
HEXA  *
HEXA       MOVE        CAD-AGENCIA TO          WRK-MESUX0-AGENCIA
HEXA       PERFORM     9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA       IF (CAD-AGENCIA            GREATER 1)     AND
HEXA          (WRK-EH-FX-AG                       )  AND
HEXA          (WRK-GRUPO-RAZAO        EQUAL   7)     AND
HEXA          (WRK-SUBGRUPO-RAZAO     NOT EQUAL 9 AND 13 AND 38 )  AND
HEXA          (WRK-SUBGRUPO-RAZAO     NOT EQUAL  50 AND 78 )       AND
HEXA          (CAD-DT-ABERT           NOT LESS WRK-DT-ABERT-OK)    AND
HEXA          (CAD-DT-ABERT           NOT GREATER WRK-DATA-FIM)    AND
HEXA          (CAD-IDENTF-CTA         NOT EQUAL '1' AND '01')      AND
HEXA          (CAD-IDENTF-CTA         NOT EQUAL ' 1' AND '1 ')     AND
HEXA          (CAD-IDENTF-CTA         NOT EQUAL '4' AND '04')      AND
HEXA          (CAD-IDENTF-CTA         NOT EQUAL ' 4' AND '4 ')
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
               PERFORM 3210-GRAVAR-CADANOVO
               PERFORM 2100-LER-CADNGERA
           ELSE
               ADD     1    TO ACU-DESPREZADOS
               PERFORM 2100-LER-CADNGERA
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3210-GRAVAR-CADANOVO            SECTION.
      *---------------------------------------------------------------*

           MOVE REG-CADACLIE           TO REG-CADANOVO.

           ADD 1                       TO ACU-GRAVADOS.
           WRITE REG-CADANOVO.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1130-TESTAR-FS-CADANOVO.

      *---------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *****************************************************************
      *   PROCESSA ROTINA DE ERRO DO PROGRAMA.                        *
      *****************************************************************
      *---------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT2090'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-CADNGERA     TO WRK-EDITADO.
           DISPLAY '**************** GFCT2090 *******************'.
           DISPLAY '*                                           *'.
           DISPLAY '*  REGS. LIDOS    (CADNGERA).: ' WRK-EDITADO '  *'.
           MOVE ACU-LIDOS-CADNGERA     TO WRK-EDITADO.
           MOVE ACU-GRAVADOS           TO WRK-EDITADO.
           DISPLAY '*  REGS. GRAVADOS (CADANOVO).: ' WRK-EDITADO '  *'.
           MOVE ACU-DESPREZADOS        TO WRK-EDITADO.
           DISPLAY '*  REGS. DESPREZADOS         : ' WRK-EDITADO '  *'.
           DISPLAY '*                                           *'.
           DISPLAY '**************** GFCT2090 *******************'.

           CLOSE  CADNGERA.
           CLOSE  CADANOVO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CADNGERA.
           PERFORM 1130-TESTAR-FS-CADANOVO.

           GOBACK.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
HEXA  *
HEXA       IF          WRK-EH-ERRO-MESU-ABEND
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA'
HEXA             TO    ERR-TEXTO
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND
HEXA       ELSE
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA
HEXA       END-IF.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       MOVE    'GFCT2090'      TO          ERR-PGM.
HEXA  *
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0
HEXA                                           ERRO-AREA.
HEXA  *
HEXA       GOBACK.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
