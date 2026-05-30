      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1739.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1739                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   21/06/2008                                  *
      *                                                                *
      *    OBJETIVO    :   EXCLUI CLIENTES QUE PAGARAM FICHA CADASTRAL *
      *                    NOS ULTIMOS SEIS MESES.                     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      SELECAO6                                    I#GFCT61      *
      *      EFICAPAG                                    I#GFCT65      *
      *      COBRANCA                                    I#GFCT61      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                                         *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *----------------------------------------------------------------*
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
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

           SELECT SELECAO6 ASSIGN      TO UT-S-SELECAO6
                      FILE STATUS      IS WRK-FS-SELECAO6.

           SELECT EFICAPAG ASSIGN      TO UT-S-EFICAPAG
                      FILE STATUS      IS WRK-FS-EFICAPAG.

           SELECT COBRANCA ASSIGN      TO UT-S-COBRANCA
                      FILE STATUS      IS WRK-FS-COBRANCA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  SELECAO6
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT61'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 039                  *
      *----------------------------------------------------------------*

       FD  EFICAPAG
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT65'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  COBRANCA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-COBRANCA          PIC X(030).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1739  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SELECAO6       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-EFICAPAG       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-COBRANCA    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC   9(002)         VALUE ZEROS.
           05  FILLER                  PIC   X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC   9(002)         VALUE ZEROS.
           05  FILLER                  PIC   X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC   9(004)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)          VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-SELECAO6.
           05 CPF-SELECAO6           PIC 9(09)           VALUE ZEROS.

       01  WRK-CHV-EFICAPAG.
           05 CPF-EFICAPAG           PIC 9(09)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SELECAO6             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EFICAPAG             PIC X(02)           VALUE SPACES.
       01  WRK-FS-COBRANCA             PIC X(02)           VALUE SPACES.
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
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA BRAD7100       *'.
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
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT1739   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-CHV-SELECAO6   EQUAL HIGH-VALUES.

           PERFORM 1400-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT SELECAO6
                      EFICAPAG
               OUTPUT COBRANCA.

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

           PERFORM 0300-TESTAR-FS-SELECAO6.

           PERFORM 0400-TESTAR-FS-EFICAPAG.

           PERFORM 0500-TESTAR-FS-COBRANCA.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SELECAO6                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-SELECAO6         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SELECAO6         NOT EQUAL '00'
               MOVE 'SELECAO6'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELECAO6    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EFICAPAG                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-EFICAPAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EFICAPAG         NOT EQUAL '00'
               MOVE 'EFICAPAG'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EFICAPAG    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO COBRANCA                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-COBRANCA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-COBRANCA         NOT EQUAL '00'
               MOVE 'COBRANCA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-COBRANCA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-SELECAO6.

           IF  WRK-CHV-SELECAO6        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1739 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SELECAO6 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1739 ***********'
           ELSE
               PERFORM 0950-ACESSAR-DPROCM
           END-IF.

           PERFORM 0900-LER-EFICAPAG.

           IF  WRK-CHV-EFICAPAG            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1739 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO EFICAPAG VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1739 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SELECAO6                                 *
      *----------------------------------------------------------------*
       0800-LER-SELECAO6               SECTION.
      *----------------------------------------------------------------*

           READ SELECAO6.

           IF  WRK-FS-SELECAO6         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SELECAO6
               GO                      TO 0800-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-SELECAO6.

           MOVE GFCT61-CPF-NUM OF SELECAO6 TO CPF-SELECAO6.

           ADD 1                       TO ACU-LIDOS-SELECAO6.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO EFICAPAG                                 *
      *----------------------------------------------------------------*
       0900-LER-EFICAPAG               SECTION.
      *----------------------------------------------------------------*

           READ EFICAPAG.

           IF  WRK-FS-EFICAPAG         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-EFICAPAG
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-EFICAPAG.

           MOVE GFCT65-CPF-NUM OF EFICAPAG TO CPF-EFICAPAG.

           ADD 1                       TO ACU-LIDOS-EFICAPAG.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTBA1 E BUSCA DATA PROCM             *
      ******************************************************************
      *----------------------------------------------------------------*
       0950-ACESSAR-DPROCM             SECTION.
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
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1      TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2                   TO WRK-DIA-INV.
           MOVE WRK-MES-DB2                   TO WRK-MES-INV.
           MOVE WRK-ANO-DB2                   TO WRK-ANO-INV.

      *----------------------------------------------------------------*
       0950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-SELECAO6      EQUAL WRK-CHV-EFICAPAG
                   ADD 1 TO ACU-DESPREZADOS
                   PERFORM 0800-LER-SELECAO6
           ELSE
               IF  WRK-CHV-SELECAO6  LESS WRK-CHV-EFICAPAG
                   PERFORM 1100-GRAVAR-COBRANCA
                   PERFORM 0800-LER-SELECAO6
               ELSE
                   PERFORM 0900-LER-EFICAPAG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  COBRANCA                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-COBRANCA            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-GFCT61 TO FD-REG-COBRANCA.

           WRITE FD-REG-COBRANCA OF COBRANCA.

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-COBRANCA.

           ADD 1                        TO ACU-GRAVADOS-COBRANCA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1400-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-EMITIR-DISPLAY.

           CLOSE SELECAO6
                 EFICAPAG
                 COBRANCA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA EMISSAO DOS DISPLAYS FINAIS DO PROGRAMA         *
      *----------------------------------------------------------------*
       1500-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT1739 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SELECAO6     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SELECAO6......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EFICAPAG     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM EFICAPAG......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-COBRANCA  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO COBRANCA...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  PGTO. JA EFETUADO......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1739 ********************'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1739'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
