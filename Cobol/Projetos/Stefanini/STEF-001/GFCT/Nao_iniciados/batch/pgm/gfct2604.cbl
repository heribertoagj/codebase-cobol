      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2604.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2604                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   14/11/2008                                  *
      *                                                                *
      *    OBJETIVO    :   BATE COM SEGUNDA LINHA DO EXTRATO OU        *
      *                    DESCRICAO GERANDO O IDENTIFICADOR.          *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                    INCLUDE/BOOK    *
      *      EXTRAMOK                                    GFCTWADJ      *
      *      SEGLINHA                                    GFCTWADL      *
      *      EXTRAMOK                                    GFCTWADM      *
      *      EXTRANOK                                    GFCTWADK      *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100  -  MODULO DE TRATAMENTO DE ERRO.                *
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

           SELECT EXTRAMOK ASSIGN      TO UT-S-EXTRAMOK
                      FILE STATUS      IS WRK-FS-EXTRAMOK.

           SELECT SEGLINHA ASSIGN      TO UT-S-SEGLINHA
                      FILE STATUS      IS WRK-FS-SEGLINHA.

           SELECT EXTRASOK ASSIGN      TO UT-S-EXTRASOK
                      FILE STATUS      IS WRK-FS-EXTRASOK.

           SELECT EXTRANOK ASSIGN      TO UT-S-EXTRANOK
                      FILE STATUS      IS WRK-FS-EXTRANOK.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 131                  *
      *----------------------------------------------------------------*

       FD  EXTRAMOK
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWADJ'.

      *----------------------------------------------------------------*
      *    INPUT:                                                      *
      *            ORG. SEQUENTIAL     -  LRECL = 071                  *
      *----------------------------------------------------------------*

       FD  SEGLINHA
           RECORDING MODE IS F
           LABEL RECORDS IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWADL'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 150                  *
      *----------------------------------------------------------------*

       FD  EXTRASOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWADM'.

      *----------------------------------------------------------------*
      *    OUTPUT:                                                     *
      *            ORG. SEQUENTIAL     -  LRECL = 171                  *
      *----------------------------------------------------------------*

       FD  EXTRANOK
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWADK'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT2604  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-EXTRAMOK       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-LIDOS-SEGLINHA       PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXTRASOK    PIC 9(11) COMP-3    VALUE ZEROS.
           05 ACU-GRAVADOS-EXTRANOK    PIC 9(11) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                       PIC X(32)           VALUE
           '*    VARIAVEIS AUXILIARES      *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC X(08)           VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZ.ZZZ.ZZZ.Z99   VALUE ZEROS.

       01  WRK-SEGLINHA-R              PIC  X(032)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-SEGLINHA-R.
           05 WRK-SEGLINHA             PIC  X(030).
           05 WRK-BRANCOS-1            PIC  X(002).

       01  WRK-SEGLINDJ-R              PIC  X(032)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-SEGLINDJ-R.
           05 WRK-SEGLINDJ             PIC  X(028).
           05 WRK-BRANCOS-4            PIC  X(004).

       01  WRK-SEGLINDJ1-R             PIC  X(032)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-SEGLINDJ1-R.
           05 WRK-SEGLINDJ1            PIC  X(028).
           05 WRK-BRANCOS-5            PIC  X(004).

       01  WRK-DESC-R                  PIC  X(032)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DESC-R.
           05 WRK-DESC                 PIC  X(025).
           05 WRK-BRANCOS-2            PIC  X(007).

       01  WRK-DESC1-R                 PIC  X(032)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DESC1-R.
           05 WRK-DESC1                PIC  X(031).
           05 WRK-BRANCOS-3            PIC  X(001).

       01 WRK-QTDE-R                   PIC  9(007)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-QTDE-R.
           05 FILLER                   PIC  9(002).
           05 WRK-QTDE                 PIC  9(005).

       01  WRK-QTDE-COMP              PIC  9(005)    COMP-3 VALUE ZEROS.
       01  WRK-VALOR-COMP             PIC  9(005)V99 COMP-3 VALUE ZEROS.
       01  WRK-VLR-LANC-COMP          PIC  9(012)V99 COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA CHAVES       *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-EXTRAMOK.
           05 FILLER                 PIC X(32)            VALUE SPACES.

       01  WRK-CHV-SEGLINHA.
           05 FILLER                 PIC X(32)            VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA PARA FILE STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EXTRAMOK             PIC X(02)           VALUE SPACES.
       01  WRK-FS-SEGLINHA             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXTRASOK             PIC X(02)           VALUE SPACES.
       01  WRK-FS-EXTRANOK             PIC X(02)           VALUE SPACES.
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
           '*   FIM  DA WORKING GFCT2604   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*

       01  LNK-PARM.
           05  FILLER                  PIC S9(04) COMP.
           05  LNK-SEGDESC             PIC  9(01).

      *===============================================================*
       PROCEDURE DIVISION USING LNK-PARM.
      *===============================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES TO WRK-BRANCOS-1
                          WRK-BRANCOS-2
                          WRK-BRANCOS-3
                          WRK-BRANCOS-4
                          WRK-BRANCOS-5.

           PERFORM 0050-CONSISTIR-PARM.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0700-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL
                    WRK-CHV-EXTRAMOK   EQUAL HIGH-VALUES.

           PERFORM 1300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0050-CONSISTIR-PARM            SECTION.
      *----------------------------------------------------------------*

           IF LNK-SEGDESC        NOT EQUAL 0 AND 1
               DISPLAY '************** GFCT2604 *************'
               DISPLAY '*  PARAMETRO INVALIDO: ' LNK-SEGDESC
                                               '            *'
               DISPLAY '*        DADOS INCONSISTENTES       *'
               DISPLAY '************** GFCT2604 *************'
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

           OPEN INPUT EXTRAMOK
                      SEGLINHA
               OUTPUT EXTRASOK
                      EXTRANOK.

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

           PERFORM 0300-TESTAR-FS-EXTRAMOK.

           PERFORM 0400-TESTAR-FS-SEGLINHA.

           PERFORM 0500-TESTAR-FS-EXTRASOK.

           PERFORM 0600-TESTAR-FS-EXTRANOK.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXTRAMOK                      *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-EXTRAMOK         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRAMOK         NOT EQUAL '00'
               MOVE 'EXTRAMOK'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRAMOK    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO SEGLINHA                      *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-SEGLINHA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGLINHA         NOT EQUAL '00'
               MOVE 'SEGLINHA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SEGLINHA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXTRAMOK                      *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-EXTRASOK         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRASOK         NOT EQUAL '00'
               MOVE 'EXTRAMOK'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRASOK    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE-STATUS DO ARQUIVO EXTRANOK                      *
      *----------------------------------------------------------------*
       0600-TESTAR-FS-EXTRANOK         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EXTRANOK         NOT EQUAL '00'
               MOVE 'EXTRANOK'         TO WRK-NOME-ARQ
               MOVE WRK-FS-EXTRANOK    TO WRK-FS
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

           PERFORM 0800-LER-EXTRAMOK.

           IF  WRK-CHV-EXTRAMOK        EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2604 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO  EXTRAMOK VAZIO   *'
               DISPLAY '*    PROCESSAMENTO ENCERRADO   *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2604 ***********'
           END-IF.

           PERFORM 0900-LER-SEGLINHA.

           IF  WRK-CHV-SEGLINHA            EQUAL HIGH-VALUES
               DISPLAY '*********** GFCT2604 ***********'
               DISPLAY '*                              *'
               DISPLAY '*    ARQUIVO SEGLINHA VAZIO    *'
               DISPLAY '*                              *'
               DISPLAY '*********** GFCT2604 ***********'
           END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO EXTRAMOK                                 *
      *----------------------------------------------------------------*
       0800-LER-EXTRAMOK               SECTION.
      *----------------------------------------------------------------*

           READ EXTRAMOK.

           IF  WRK-FS-EXTRAMOK           EQUAL '10'
               MOVE HIGH-VALUES          TO WRK-CHV-EXTRAMOK
               GO                        TO 0800-99-FIM
           END-IF.

           IF LNK-SEGDESC EQUAL 1
               MOVE WADJ-HIST-CPL        TO WRK-SEGLINDJ-R
               MOVE WRK-SEGLINDJ         TO WRK-SEGLINDJ1
               MOVE WRK-SEGLINDJ1-R      TO WRK-CHV-EXTRAMOK
                                            WADM-SEGUNDA-LINHA
           ELSE
               MOVE WADJ-DESC-LANCAMENTO TO WRK-DESC1
               MOVE WRK-DESC1-R          TO WRK-CHV-EXTRAMOK
                                            WADM-SEGUNDA-LINHA
           END-IF.

           MOVE GFCTWADJ-REG             TO GFCTWADK-REG.
           MOVE SPACES                   TO WADK-DESC-ERRO.
           MOVE WADJ-TIPO-REGISTRO       TO WADM-TIPO-REGISTRO.
           MOVE WADJ-COD-AGENCIA         TO WADM-COD-AGENCIA.
           MOVE WADJ-CPF-CNPJ            TO WADM-CPF-CNPJ.
           MOVE WADJ-RAZAO               TO WADM-RAZAO.
           MOVE WADJ-NRO-CONTA           TO WADM-NRO-CONTA.
           MOVE WADJ-NUMERO-DOCUMENTO    TO WADM-NUMERO-DOCUMENTO.
           MOVE WADJ-COD-LANCAMENTO      TO WADM-COD-LANCAMENTO.
           MOVE WADJ-DATA-LANCAMENTO     TO WADM-DATA-LANCAMENTO.
           MOVE WADJ-VLR-LANCAMENTO      TO WADM-VLR-LANCAMENTO.

           MOVE WRK-LEITURA              TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-EXTRAMOK.

           ADD 1                         TO ACU-LIDOS-EXTRAMOK.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DO ARQUIVO SEGLINHA                                 *
      *----------------------------------------------------------------*
       0900-LER-SEGLINHA               SECTION.
      *----------------------------------------------------------------*

           READ SEGLINHA.

           IF  WRK-FS-SEGLINHA         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-SEGLINHA
               GO                      TO 0900-99-FIM
           END-IF.

           IF LNK-SEGDESC EQUAL 1
               MOVE WADL-SEGUNDA-LINHA     TO WRK-SEGLINHA
               MOVE WRK-SEGLINHA-R         TO WRK-CHV-SEGLINHA
           ELSE
               MOVE WADL-DESCRICAO         TO WRK-DESC
               MOVE WRK-DESC-R             TO WRK-CHV-SEGLINHA
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-SEGLINHA.

           ADD 1                       TO ACU-LIDOS-SEGLINHA.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-EXTRAMOK      EQUAL WRK-CHV-SEGLINHA
               PERFORM 1100-GRAVAR-EXTRASOK
               PERFORM 0800-LER-EXTRAMOK
           ELSE
               IF  WRK-CHV-EXTRAMOK  LESS WRK-CHV-SEGLINHA
                   MOVE 'NAO ENCONTRADO SEGUNDA LINHA' TO WADK-DESC-ERRO
                   PERFORM 1200-GRAVAR-EXTRANOK
                   PERFORM 0800-LER-EXTRAMOK
               ELSE
                   PERFORM 0900-LER-SEGLINHA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXTRASOK                                    *
      *----------------------------------------------------------------*
       1100-GRAVAR-EXTRASOK            SECTION.
      *----------------------------------------------------------------*
      * ST25X6* Ajuste ST: CPF/CNPJ alfanumérico (PIC X(009)).
      * ST25X6* Não aplicar checagem "NOT NUMERIC"; consistir BRANCO/ZEROS.
           IF WADM-CPF-CNPJ          EQUAL SPACES
               MOVE 'CPF/CNPJ EM BRANCO' TO WADK-DESC-ERRO
               PERFORM 1200-GRAVAR-EXTRANOK
               GO TO 1100-99-FIM
           END-IF.

           IF WADM-CPF-CNPJ          EQUAL ZEROS
               MOVE 'CPF/CNPJ ZERADO'    TO WADK-DESC-ERRO
               PERFORM 1200-GRAVAR-EXTRANOK
               GO TO 1100-99-FIM
           END-IF.

           IF WADJ-TIPO-REGISTRO     EQUAL 'E'
               MOVE         4        TO WADM-IDENT
           ELSE
               IF WADL-GRUPO         EQUAL 'CS'
                   MOVE     1        TO WADM-IDENT
               ELSE
                   IF WADL-GRUPO     EQUAL 'SE'
                       MOVE 2        TO WADM-IDENT
                       PERFORM 1150-TRATAR-QTDE
                   ELSE
                       MOVE 3        TO WADM-IDENT
                       PERFORM 1150-TRATAR-QTDE
                   END-IF
               END-IF
           END-IF.

           INITIALIZE                     GFCTWADK-REG.

           WRITE GFCTWADM-REG          OF EXTRASOK.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-EXTRASOK.

           ADD 1                       TO ACU-GRAVADOS-EXTRASOK.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATAR QUANTIDADE                                           *
      *----------------------------------------------------------------*
       1150-TRATAR-QTDE            SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                       TO WRK-QTDE-R.
           MOVE ZEROS                       TO WRK-QTDE-COMP.

           IF WADJ-RAZAO    EQUAL    '010051'
               IF WADL-ID-SE-POUP EQUAL     'N'
                   MOVE WADJ-NUMERO-DOCUMENTO   TO WRK-QTDE-R
                   MOVE WRK-QTDE                TO WADM-QTDE-TAR
               ELSE
                   IF WADL-ID-SE-POUP EQUAL 'V'
                       MOVE WADL-ID-SE-VALOR    TO WRK-VALOR-COMP
                       MOVE WADJ-VLR-LANCAMENTO TO WRK-VLR-LANC-COMP
                       COMPUTE WRK-QTDE-COMP ROUNDED =
                       WRK-VLR-LANC-COMP / WRK-VALOR-COMP
                       MOVE WRK-QTDE-COMP   TO WADM-QTDE-TAR
                   ELSE
                       MOVE 1                   TO WADM-QTDE-TAR
                   END-IF
               END-IF
           ELSE
               IF WADL-ID-SE-CC EQUAL     'N'
                   MOVE WADJ-NUMERO-DOCUMENTO   TO WRK-QTDE-R
                   MOVE WRK-QTDE                TO WADM-QTDE-TAR
               ELSE
                   IF WADL-ID-SE-CC EQUAL 'V'
                       MOVE WADL-ID-SE-VALOR    TO WRK-VALOR-COMP
                       MOVE WADJ-VLR-LANCAMENTO TO WRK-VLR-LANC-COMP
                       COMPUTE WRK-QTDE-COMP ROUNDED =
                       WRK-VLR-LANC-COMP / WRK-VALOR-COMP
                       MOVE WRK-QTDE-COMP   TO WADM-QTDE-TAR
                    ELSE
                       MOVE 1                   TO WADM-QTDE-TAR
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO  EXTRANOK                                    *
      *----------------------------------------------------------------*
       1200-GRAVAR-EXTRANOK            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     GFCTWADM-REG.

           WRITE GFCTWADK-REG          OF EXTRANOK.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-EXTRANOK.

           ADD 1                       TO ACU-GRAVADOS-EXTRANOK.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-EMITIR-DISPLAY.

           CLOSE EXTRAMOK
                 SEGLINHA
                 EXTRASOK
                 EXTRANOK.

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

           DISPLAY '******************** GFCT2604 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EXTRAMOK     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM EXTRAMOK......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-SEGLINHA     TO WRK-MASCARA.
           DISPLAY '*  LIDOS EM SEGLINHA......: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXTRASOK  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO EXTRAMOK...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVADOS-EXTRANOK  TO WRK-MASCARA.
           DISPLAY '*  GRAVADOS NO EXTRANOK...: '
                                                WRK-MASCARA  '       *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT2604 ********************'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2604'             TO ERR-PGM.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------
