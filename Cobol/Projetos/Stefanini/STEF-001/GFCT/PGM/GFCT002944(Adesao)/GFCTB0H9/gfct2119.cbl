      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2119.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2119                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   17/08/2009                                  *
      *                                                                *
      *    OBJETIVO    :   ASSOCIA PACOTE INDICADO NO PARM, SENDO 'F'  *
      *                    PACOTE PARA PESSOA FISICA E 'J' PARA PESSOA *
      *                    JURIDICA.                                   *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      POUPADE2                                    I#GFCTRU      *
      *      POUPADES                                    I#GFCTRZ      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *----------------------------------------------------------------*
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *    BRAD7100 - MODULO DE TRATAMENTO DE ERRO.                    *
      *    BRAD0025 - SOMA DIAS NA DATA.                               *
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

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT POUPADE2 ASSIGN      TO UT-S-POUPADE2
                      FILE STATUS      IS WRK-FS-POUPADE2.

           SELECT POUPADES ASSIGN      TO UT-S-POUPADES
                      FILE STATUS      IS WRK-FS-POUPADES.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  POUPADE2
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 050                  *
      *----------------------------------------------------------------*

       FD  POUPADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRZ'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2119  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-POUPADE2       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-POUPADES    PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA                 PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES                 PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO                 PIC  9(004) VALUE ZEROS.

       01  WRK-MES-COMP                PIC  9(002) COMP-3 VALUE ZEROS.
       01  WRK-ANO-COMP                PIC  9(004) COMP-3 VALUE ZEROS.

       01  WRK-DATA-0025               PIC  9(009)        VALUE ZEROS.
       01  WRK-DATA-0025-R             REDEFINES WRK-DATA-0025.
           05  FILLER                  PIC  9(001).
           05  WRK-ANO-0025            PIC  9(004).
           05  WRK-MES-0025            PIC  9(002).
           05  WRK-DIA-0025            PIC  9(002).

       01  WRK-DATA-INICIO             PIC  9(009)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-INICIO.
           05  FILLER                  PIC  9(001).
           05  WRK-ANO-INI             PIC  9(004).
           05  WRK-MES-INI             PIC  9(002).
           05  WRK-DIA-INI             PIC  9(002).

       01  WRK-DATA-FIM                PIC  9(009)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-FIM.
           05  FILLER                  PIC  9(001).
           05  WRK-ANO-FIM             PIC  9(004).
           05  WRK-MES-FIM             PIC  9(002).
           05  WRK-DIA-FIM             PIC  9(002).

       01  WRK-S9-5                    PIC S9(005)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S9-5.
           05  WRK-9-5                 PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-POUPADE2             PIC X(02)           VALUE SPACES.
       01  WRK-FS-POUPADES             PIC X(02)           VALUE SPACES.
       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           'NA  LEITURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA  DE MENSAGEM  DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(07)           VALUE SPACES.
           05  FILLER                  PIC X(07)           VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           05  FILLER                  PIC X(12)           VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(08)           VALUE SPACES.
           05  FILLER                  PIC X(17)           VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(02)           VALUE SPACES.
           05  FILLER                  PIC X(02)           VALUE ' *'.
           05  FILLER                  PIC X(07)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA BRAD0025'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA            PIC   9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-DIAS             PIC  S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC   9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA TABELA DB2'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2119   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           05  FILLER                  PIC S9(04) COMP.
           05  LNK-TIPO-PESSOA-F       PIC  X(01).
           05  LNK-PACOTE-F            PIC  9(05).
           05  FILLER                  PIC  X(01).
           05  LNK-TIPO-PESSOA-J       PIC  X(01).
           05  LNK-PACOTE-J            PIC  9(05).

      *===============================================================*
       PROCEDURE DIVISION USING LNK-PARM.
      *===============================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0050-CONSISTIR-PARM.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 0900-OBTER-DATA-SISTEMA.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-FS-POUPADE2    EQUAL '10'.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0050-CONSISTIR-PARM            SECTION.
      *----------------------------------------------------------------*

           IF LNK-TIPO-PESSOA-F        NOT EQUAL 'F' OR
              LNK-TIPO-PESSOA-J        NOT EQUAL 'J' OR
              LNK-PACOTE-F             NOT NUMERIC   OR
              LNK-PACOTE-J             NOT NUMERIC
               DISPLAY '************** GFCT2119 *************'
               DISPLAY '*        PARAMETRO INVALIDO:        *'
               DISPLAY '* FISICA  : '
                                    LNK-TIPO-PESSOA-F'-'LNK-PACOTE-F
                                          '                 *'
               DISPLAY '* JURIDICA: '
                                    LNK-TIPO-PESSOA-J'-'LNK-PACOTE-J
                                          '                 *'
               DISPLAY '*        DADOS INCONSISTENTES       *'
               DISPLAY '************** GFCT2119 *************'
               MOVE  'APL'                  TO  ERR-TIPO-ACESSO
               MOVE  'PARM'                 TO  WRK-NOME-ARQ
               MOVE  'INCONSISTENTE'        TO  ERR-TEXTO
               PERFORM  9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  POUPADE2
                OUTPUT POUPADES.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DOS ARQUIVOS DE ENTRADA E SAIDA          *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-POUPADE2.

           PERFORM 0600-TESTAR-FS-POUPADES.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPADE2                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-POUPADE2         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-POUPADE2         NOT EQUAL '00'
               MOVE 'POUPADE2'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPADE2    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPADES                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-POUPADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-POUPADES         NOT EQUAL '00'
               MOVE 'POUPADES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPADES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-POUPADE2.

           IF  WRK-FS-POUPADE2        EQUAL '10'
               DISPLAY '*********** GFCT2119 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  POUPADE2 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2119 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO POUPADE2                                 *
      *----------------------------------------------------------------*
       0800-LER-POUPADE2               SECTION.
      *----------------------------------------------------------------*

           READ POUPADE2.

           IF  WRK-FS-POUPADE2           EQUAL '10'
               GO                        TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA              TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-POUPADE2.

           ADD 1                         TO ACU-LIDOS-POUPADE2.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER DADOS DA TABELA GFCTB0A1                     *
      *----------------------------------------------------------------*
       0900-OBTER-DATA-SISTEMA               SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

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
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1      TO WRK-DATA-DB2.
           MOVE WRK-DIA                       TO WRK-DIA-0025.
           MOVE WRK-MES                       TO WRK-MES-0025.
           MOVE WRK-ANO                       TO WRK-ANO-0025.

******** OBTER A DATA INICIO.

           MOVE 01                            TO WRK-DIA-INI.
           MOVE WRK-MES                       TO WRK-MES-INI.
           MOVE WRK-ANO                       TO WRK-ANO-INI.

******** OBTER A DATA FIM.

           MOVE 01                            TO WRK-DIA-0025.

           MOVE WRK-ANO-0025                  TO WRK-ANO-COMP.
           MOVE WRK-MES-0025                  TO WRK-MES-COMP.

           IF WRK-MES-0025 EQUAL 12
               ADD  1            TO WRK-ANO-COMP
               MOVE WRK-ANO-COMP TO WRK-ANO-0025
               MOVE 01           TO WRK-MES-0025
           ELSE
               ADD 1             TO WRK-MES-COMP
               MOVE WRK-MES-COMP TO WRK-MES-0025
           END-IF.

           MOVE WRK-DATA-0025          TO WRK-DATA-ENTRADA.
           MOVE -01                    TO WRK-NUMERO-DIAS.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'ERRO NA BRAD0025 - DADOS INCONSISTENTES'
                                       TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-DATA-0025.
           MOVE WRK-DIA-0025           TO WRK-DIA-FIM.
           MOVE WRK-MES-0025           TO WRK-MES-FIM.
           MOVE WRK-ANO-0025           TO WRK-ANO-FIM.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA       TO WRK-S9-5.
           MOVE WRK-9-5              TO GFCTRZ-AGENCIA.
           MOVE 10510                TO GFCTRZ-RAZAO.
           MOVE GFCTRU-CONTA         TO GFCTRZ-CONTA.
           MOVE GFCTRU-POSTO-SERV    TO GFCTRZ-PAB.
           MOVE '01.01.0001'         TO GFCTRZ-DATA.
           MOVE WRK-DATA-INICIO      TO GFCTRZ-DINI.
           MOVE WRK-DATA-FIM         TO GFCTRZ-DFIM.
           MOVE 25                   TO GFCTRZ-AGPTO.
           MOVE ZEROS                TO GFCTRZ-SEQ.

           IF GFCTRU-FILIAL EQUAL ZEROS
               MOVE LNK-PACOTE-F TO GFCTRZ-PACOTE
           ELSE
               MOVE LNK-PACOTE-J TO GFCTRZ-PACOTE
           END-IF.

           PERFORM 1200-GRAVAR-POUPADES.

           PERFORM 0800-LER-POUPADE2.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  POUPADES                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-POUPADES            SECTION.
      *----------------------------------------------------------------*

           WRITE GFCTRZ-ADESOES        OF POUPADES.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-POUPADES.

           ADD 1                       TO ACU-GRAVADOS-POUPADES.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE POUPADE2
                 POUPADES.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT2119 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '*        PROGRAMA ENCERRADO COM SUCESSO          *'.
           DISPLAY '*   ==========================================   *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-POUPADE2     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM POUPADE2......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-POUPADES  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO POUPADES...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2119 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2119'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

