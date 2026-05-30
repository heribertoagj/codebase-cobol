      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2117.
       AUTHOR.     DIEGO PAGAN.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2117                                    *
      *    PROGRAMADOR.:   DIEGO PAGAN - SONDPROC/GP.50                *
      *    ANALISTA....:   DIEGO PAGAN - SONDPROC/GP.50                *
      *    DATA........:   14/08/2009                                  *
      *                                                                *
      *    OBJETIVO    :   IDENTIFICAR O CADASTRO DE POUPANCA, AGENCIAS*
      *                    INVALIDAS.                                  *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      CADAPOUP                                    GFCTWAIR      *
      *      POUPADES                                    I#GFCTRU      *
      *      POUPINVA                                    GFCTWAIR      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
      *      MESU9400  -  MODULO DE PESQUISA DE JUNCOES.               *
      *                                                                *
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

           SELECT CADAPOUP ASSIGN      TO UT-S-CADAPOUP
                      FILE STATUS      IS WRK-FS-CADAPOUP.

           SELECT POUPADES ASSIGN      TO UT-S-POUPADES
                      FILE STATUS      IS WRK-FS-POUPADES.

           SELECT POUPINVA ASSIGN      TO UT-S-POUPINVA
                      FILE STATUS      IS WRK-FS-POUPINVA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 007                  *
      *----------------------------------------------------------------*

       FD  CADAPOUP
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAIR'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 040                  *
      *----------------------------------------------------------------*

       FD  POUPADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 007                  *
      *----------------------------------------------------------------*

       FD  POUPINVA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAIR'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2117  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-CADAPOUP       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-POUPADES    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99  VALUE ZEROS.

       01  WRK-CADAPOUP-ATU.
           03 WRK-AGENCIA-ATU          PIC 9(005)          VALUE ZEROS.

       01  WRK-CADAPOUP-ANT.
           03 WRK-AGENCIA-ANT          PIC 9(005)          VALUE ZEROS.

       01  WRK-FLAG-AGENCIA            PIC X(001)          VALUE 'N'.
       01  WRK-MODULO                  PIC X(008)          VALUE SPACES.

       01  WRK-S9-5                    PIC S9(005)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S9-5.
           05  WRK-9-5                 PIC  9(005).

       01  WRK-S9-7                    PIC S9(007)        VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S9-7.
           05  WRK-9-7                 PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    MODULO MESU9410      *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-MESU9410.
           03 WRK-MESU9410-ENTRADA.
              05 WRK-MESU9410-BANCO    PIC 9(003) COMP-3   VALUE 237.
              05 WRK-MESU9410-AGENCIA  PIC 9(005) COMP-3   VALUE ZEROS.
           03 WRK-MESU9410-SAIDA.
              05 WRK-MESU9410-RETORNO  PIC 9(002)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-CADAPOUP             PIC X(02)           VALUE SPACES.
       01  WRK-FS-POUPADES             PIC X(02)           VALUE SPACES.
       01  WRK-FS-POUPINVA             PIC X(02)           VALUE SPACES.
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
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM  DA WORKING GFCT2117   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-FS-CADAPOUP    EQUAL '10'.

           PERFORM 1400-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA INICIALIZAR O PROGRAMA                          *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT CADAPOUP
               OUTPUT POUPADES
                      POUPINVA.

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

           PERFORM 0300-TESTAR-FS-CADAPOUP.

           PERFORM 0400-TESTAR-FS-POUPADES.

           PERFORM 0500-TESTAR-FS-POUPINVA.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO CADAPOUP                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-CADAPOUP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADAPOUP         NOT EQUAL '00'
               MOVE 'CADAPOUP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-CADAPOUP    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPADES                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-POUPADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-POUPADES         NOT EQUAL '00'
               MOVE 'POUPADES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPADES    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO POUPINVA                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-POUPINVA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-POUPINVA         NOT EQUAL '00'
               MOVE 'POUPINVA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-POUPINVA    TO WRK-FS
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

           PERFORM 0900-LER-CADAPOUP.

           IF  WRK-FS-CADAPOUP            EQUAL '10'
               DISPLAY '*********** GFCT2117 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO CADAPOUP VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2117 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO CADAPOUP                                 *
      *----------------------------------------------------------------*
       0900-LER-CADAPOUP               SECTION.
      *----------------------------------------------------------------*

           READ CADAPOUP.

           IF  WRK-FS-CADAPOUP          EQUAL '10'
               GO                       TO 0900-99-FIM
           END-IF.

           MOVE WRK-LEITURA             TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-CADAPOUP.

           MOVE POUP-AGENCIA OF CADAPOUP TO WRK-S9-5.
           MOVE WRK-9-5                  TO WRK-AGENCIA-ATU.

           ADD 1                        TO ACU-LIDOS-CADAPOUP.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-AGENCIA-ATU NOT GREATER ZEROS
               MOVE 'N' TO WRK-FLAG-AGENCIA
           ELSE
               IF WRK-AGENCIA-ATU NOT EQUAL WRK-AGENCIA-ANT
                   PERFORM 1100-VALIDAR-AGENCIA-MESU9410
                   MOVE WRK-AGENCIA-ATU TO WRK-AGENCIA-ANT
               END-IF
           END-IF.

           IF WRK-FLAG-AGENCIA EQUAL 'S'
               PERFORM 1200-GRAVAR-POUPADES
           ELSE
               PERFORM 1300-GRAVAR-POUPINVA
           END-IF

           PERFORM 0900-LER-CADAPOUP.

      *----------------------------------------------------------------*
       1000-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MODULO DE VALIDACAO DE JUNCOES                              *
      *----------------------------------------------------------------*
       1100-VALIDAR-AGENCIA-MESU9410            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-ATU             TO WRK-MESU9410-AGENCIA.
           MOVE 'MESU9410'                  TO WRK-MODULO.

           CALL  WRK-MODULO                 USING WRK-AREA-MESU9410.

           IF RETURN-CODE NOT EQUAL ZEROS
               DISPLAY '*********** GFCT2117 ***********'
               DISPLAY '*                              *'
               DISPLAY '*       ERRO NO ACESSO AO      *'
               DISPLAY '*        MODULO MESU9410       *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2117 ***********'
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF WRK-MESU9410-RETORNO EQUAL 99
               DISPLAY '*********** GFCT2117 ***********'
               DISPLAY '*                              *'
               DISPLAY '*       ERRO NA MESU9410       *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2117 ***********'
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF WRK-MESU9410-RETORNO(1:1) NOT EQUAL 0
               MOVE 'N' TO WRK-FLAG-AGENCIA
           ELSE
               MOVE 'S' TO WRK-FLAG-AGENCIA
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  POUPADES                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-POUPADES            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO                TO WRK-OPERACAO.

           MOVE SPACES                       TO GFCTRU-ENTRADA.
           MOVE POUP-AGENCIA    OF CADAPOUP  TO GFCTRU-AGENCIA.
           MOVE ZEROS                        TO GFCTRU-RAZAO.
           MOVE POUP-CONTA      OF CADAPOUP  TO WRK-S9-7.
           MOVE WRK-9-7                      TO GFCTRU-CONTA.
           MOVE ZEROS                        TO GFCTRU-CGC-CPF.
           MOVE ZEROS                        TO GFCTRU-FILIAL.
           MOVE ZEROS                        TO GFCTRU-CONTR.
           MOVE ZEROS                        TO GFCTRU-POSTO-SERV.
           MOVE ZEROS                        TO GFCTRU-DT-ABERT.

           WRITE GFCTRU-ENTRADA OF POUPADES.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-POUPADES.

           ADD 1                  TO ACU-GRAVADOS-POUPADES.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  POUPINVA                                    *
      *----------------------------------------------------------------*
       1300-GRAVAR-POUPINVA            SECTION.
      *----------------------------------------------------------------*

           MOVE REG-CADAPOUP  OF CADAPOUP TO REG-CADAPOUP OF POUPINVA.

           WRITE REG-CADAPOUP OF POUPINVA.

           MOVE WRK-GRAVACAO      TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-POUPINVA.

           ADD 1                  TO ACU-DESPREZADOS.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1400-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-EMITIR-DISPLAY.

           CLOSE CADAPOUP
                 POUPADES
                 POUPINVA.

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

           DISPLAY '******************** GFCT2117 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-CADAPOUP     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM CADAPOUP......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-POUPADES  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO POUPADES...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '*  POUPNTES DESPREZADOS...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2117 ********************'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2117'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------

