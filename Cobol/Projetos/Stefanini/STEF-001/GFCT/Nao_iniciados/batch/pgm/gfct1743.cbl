      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1743.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1743                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   21/06/2008                                  *
      *                                                                *
      *    OBJETIVO    :   TRATA CLIENTES QUE POSSUEM E NAO POSSUEM    *
      *                    CARTEIRA NO RESP.
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      ARQRESP5                                    I#GFCTU7      *
      *      SELECAO6                                    I#GFCT61      *
      *      COBRANCA                                    I#GFCT61      *
      *      COBRSBAT                                    I#GFCT66      *
      *      SFICAPAG                                    I#GFCT65      *
      *      NAOCOBRA                                    I#GFCT61      *
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

           SELECT ARQRESP5 ASSIGN      TO UT-S-ARQRESP5
                      FILE STATUS      IS WRK-FS-ARQRESP5.

           SELECT SELECAO6 ASSIGN      TO UT-S-SELECAO6
                      FILE STATUS      IS WRK-FS-SELECAO6.

           SELECT COBRANCA ASSIGN      TO UT-S-COBRANCA
                      FILE STATUS      IS WRK-FS-COBRANCA.

           SELECT COBRSBAT ASSIGN      TO UT-S-COBRSBAT
                      FILE STATUS      IS WRK-FS-COBRSBAT.

           SELECT SFICAPAG ASSIGN      TO UT-S-SFICAPAG
                      FILE STATUS      IS WRK-FS-SFICAPAG.

           SELECT NAOCOBRA ASSIGN      TO UT-S-NAOCOBRA
                      FILE STATUS      IS WRK-FS-NAOCOBRA.

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
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  ARQRESP5
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTU7'.

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
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 045                  *
      *----------------------------------------------------------------*

       FD  COBRSBAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT66'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 039                  *
      *----------------------------------------------------------------*

       FD  SFICAPAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT65'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 030                  *
      *----------------------------------------------------------------*

       FD  NAOCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-NAOCOBRA          PIC X(030).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1743  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SELECAO6       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-ARQRESP5       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-COBRANCA    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-COBRSBAT    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-SFICAPAG    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-NAOCOBRA    PIC 9(11) COMP-3    VALUE ZEROS.

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

       01  WRK-CONTA                   PIC  9(013)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-CONTA.
           05  FILLER                  PIC  9(006).
           05  WRK-GFCT-CONTA          PIC  9(007).

       01  WRK-CHV-SELECAO6.
           05 CPF-SELECAO6           PIC 9(09)           VALUE ZEROS.

       01  WRK-CHV-ARQRESP5.
           05 CPF-ARQRESP5           PIC 9(09)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-SELECAO6             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ARQRESP5             PIC X(02)           VALUE SPACES.
       01  WRK-FS-COBRANCA             PIC X(02)           VALUE SPACES.
       01  WRK-FS-COBRSBAT             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SFICAPAG             PIC X(02)           VALUE SPACES.
       01  WRK-FS-NAOCOBRA             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT1743   *'.
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
                      ARQRESP5
               OUTPUT COBRANCA
                      COBRSBAT
                      SFICAPAG
                      NAOCOBRA.

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

           PERFORM 0350-TESTAR-FS-ARQRESP5.

           PERFORM 0400-TESTAR-FS-COBRANCA.

           PERFORM 0450-TESTAR-FS-COBRSBAT.

           PERFORM 0500-TESTAR-FS-SFICAPAG.

           PERFORM 0550-TESTAR-FS-NAOCOBRA.

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
      *    TESTAR FILE-STATUS DO ARQUIVO ARQRESP5                      *
      *----------------------------------------------------------------*
       0350-TESTAR-FS-ARQRESP5         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQRESP5         NOT EQUAL '00'
               MOVE 'ARQRESP5'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQRESP5    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO COBRANCA                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-COBRANCA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-COBRANCA         NOT EQUAL '00'
               MOVE 'COBRANCA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-COBRANCA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO COBRSBAT                      *
      *----------------------------------------------------------------*
       0450-TESTAR-FS-COBRSBAT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-COBRSBAT         NOT EQUAL '00'
               MOVE 'COBRSBAT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-COBRSBAT    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SFICAPAG                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-SFICAPAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SFICAPAG         NOT EQUAL '00'
               MOVE 'SFICAPAG'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SFICAPAG    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO NAOCOBRA                      *
      *----------------------------------------------------------------*
       0550-TESTAR-FS-NAOCOBRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-NAOCOBRA         NOT EQUAL '00'
               MOVE 'NAOCOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-NAOCOBRA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA P/ VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS  *
      *----------------------------------------------------------------*
       0700-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-LER-SELECAO6.

           IF  WRK-CHV-SELECAO6        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1743 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  SELECAO6 VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1743 ***********'
           ELSE
               PERFORM 0950-ACESSAR-DPROCM
           END-IF.

           PERFORM 0900-LER-ARQRESP5.

           IF  WRK-CHV-ARQRESP5            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT1743 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO ARQRESP5 VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT1743 ***********'
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

           MOVE GFCT61-CPF-NUM         TO CPF-SELECAO6.

           ADD 1                       TO ACU-LIDOS-SELECAO6.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO ARQRESP5                                 *
      *----------------------------------------------------------------*
       0900-LER-ARQRESP5               SECTION.
      *----------------------------------------------------------------*

           READ ARQRESP5.

           IF  WRK-FS-ARQRESP5         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-ARQRESP5
               GO                      TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0350-TESTAR-FS-ARQRESP5.

           MOVE GFCTU7-CPF-NUM         TO CPF-ARQRESP5.

           ADD 1                       TO ACU-LIDOS-ARQRESP5.

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

           IF  WRK-CHV-SELECAO6      EQUAL WRK-CHV-ARQRESP5
                   PERFORM 1100-GRAVAR-COBRANCA
                   PERFORM 1150-GRAVAR-COBRSBAT
                   PERFORM 1200-GRAVAR-SFICAPAG
                   PERFORM 0800-LER-SELECAO6
           ELSE
               IF WRK-CHV-SELECAO6  LESS  WRK-CHV-ARQRESP5
                   PERFORM 1250-GRAVAR-NAOCOBRA
                   PERFORM 0800-LER-SELECAO6
               ELSE
                   PERFORM 0900-LER-ARQRESP5
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

           PERFORM 0400-TESTAR-FS-COBRANCA.

           ADD 1                        TO ACU-GRAVADOS-COBRANCA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  COBRSBAT                                    *
      *----------------------------------------------------------------*
       1150-GRAVAR-COBRSBAT            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT61-AGENCIA         TO GFCT66-AGENCIA.
           MOVE GFCT61-CONTA           TO GFCT66-CONTA.
           MOVE GFCT61-CPF-NUM         TO GFCT66-CPF-NUM.
           MOVE GFCT61-CPF-CTR         TO GFCT66-CPF-CTR.
           MOVE GFCT61-DT-ABERTURA-CTA TO GFCT66-DT-ABERTURA-CTA.
           MOVE GFCT61-TIPO-TRATO      TO GFCT66-TIPO-TRATO.

           WRITE REG-GFCT66       OF COBRSBAT.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0450-TESTAR-FS-COBRSBAT.

           ADD 1                  TO ACU-GRAVADOS-COBRSBAT.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  SFICAPAG                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-SFICAPAG            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT61-AGENCIA TO GFCT65-AGENCIA.
           MOVE GFCT61-CONTA   TO WRK-CONTA.
           MOVE WRK-GFCT-CONTA TO GFCT65-CONTA.
           MOVE GFCT61-CPF-NUM TO GFCT65-CPF-NUM.
           MOVE GFCT61-CPF-CTR TO GFCT65-CPF-DIG.
           MOVE WRK-DATA-INV   TO GFCT65-DT-INCL-RENOV
                                  GFCT65-DT-CONTROLE.

           WRITE REG-GFCT65 OF SFICAPAG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-SFICAPAG.

           ADD 1                       TO ACU-GRAVADOS-SFICAPAG.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  NAOCOBRA                                    *
      *----------------------------------------------------------------*
       1250-GRAVAR-NAOCOBRA            SECTION.
      *----------------------------------------------------------------*

           MOVE  REG-GFCT61 TO FD-REG-NAOCOBRA.

           WRITE FD-REG-NAOCOBRA OF NAOCOBRA.

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           PERFORM 0550-TESTAR-FS-NAOCOBRA.

           ADD 1                        TO ACU-GRAVADOS-NAOCOBRA.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1400-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-EMITIR-DISPLAY.

           CLOSE SELECAO6
                 ARQRESP5
                 COBRANCA
                 COBRSBAT
                 SFICAPAG
                 NAOCOBRA.

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

           DISPLAY '******************** GFCT1743 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SELECAO6     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SELECAO6......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ARQRESP5     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM ARQRESP5......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-COBRANCA  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO COBRANCA...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-COBRSBAT  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO COBRSBAT...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-SFICAPAG  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO SFICAPAG...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-NAOCOBRA  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO NAOCOBRA...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1743 ********************'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1743'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
