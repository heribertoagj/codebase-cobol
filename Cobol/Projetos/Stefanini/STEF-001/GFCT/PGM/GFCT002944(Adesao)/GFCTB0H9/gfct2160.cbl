      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2160.
       AUTHOR.     FERNANDO BATTISTI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2160                                    *
      *    PROGRAMADORA:   FERNANDO BATTISTI       - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   RICARDO PEREIRA         - PROCWORK / GP.50  *
      *    DATA........:   29/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   OBTER DADOS DO SEGMENTO GFCT A PARTIR       *
      *      DA AGENCIA E CONTA.                                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    CLIEADES                  I#GFCTRU          *
      *                    CLIESEG                   I#GFCTRY          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100.     *
      *    I#GFCTRU - ARQUIVO DE ENTRADA.                              *
      *    I#GFCTRY - ARQUIVO DE SAIDA.                                *
      *    I#GFCTJM - AREA DE COMUNICACAO COM O MODULO GFCT5534.       *
      *    I#GFCT0M - AREA DE COMUNICACAO COM O MODULO GFCT5534        *
      *               PARA TRATAMENTO DE ERRO.                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5534 - OBTEM CODIGO DE DEPENDENCIA DE GESTAO DO         *
      *               SEGMENTO DA TARIFA.                              *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
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

           SELECT CLIEADES ASSIGN      TO UT-S-CLIEADES
           FILE STATUS                 IS WRK-FS-CLIEADES.

           SELECT CLIESEG  ASSIGN      TO UT-S-CLIESEG
           FILE STATUS                 IS WRK-FS-CLIESEG.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO DE CLIENTES - ENTRADA                       *
      *            ORG. SEQUENCIAL     -   LRECL = 040                 *
      *----------------------------------------------------------------*

       FD  CLIEADES
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRU'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CLIENTES - SAIDA                         *
      *            ORG. SEQUENCIAL     -   LRECL = 050                 *
      *----------------------------------------------------------------*

       FD  CLIESEG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTRY'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CLIEADES          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-CLIE-PROCESSADO         PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-CLIE-NAO-CADASTRADO     PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-CLIESEG        PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-CLIEADES             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CLIESEG              PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA-AGENCIA         PIC  9(005)         VALUE ZEROS.
       77  WRK-MASCARA-CONTA           PIC  9(007)         VALUE ZEROS.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(058)         VALUE
           '** ARQUIVO CLIEADES ESTA VAZIO, PROCESSAMENTO CANCELADO **'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM O MODULO GFCT5534 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTJM'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5534 PARA ERRO *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-CLIEADES     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DE ARQUIVOS                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  CLIEADES
                OUTPUT CLIESEG.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-CLIEADES.

           PERFORM 1120-TESTAR-FS-CLIESEG.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CLIEADES                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CLIEADES         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIEADES         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIEADES    TO WRK-FILE-STATUS
               MOVE 'CLIEADES'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO CLIESEG                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CLIESEG          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIESEG          NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CLIESEG     TO WRK-FILE-STATUS
               MOVE 'CLIESEG '         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               DISPLAY '*************** GFCT2160 ***************'
               DISPLAY '*                                      *'
               DISPLAY '*      ARQUIVO CLIEADES ESTA VAZIO     *'
               DISPLAY '*                                      *'
               DISPLAY '*        PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                      *'
               DISPLAY '*************** GFCT2160 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG001         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO DE PARAMETROS CLIEADES                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-CLIEADES               SECTION.
      *----------------------------------------------------------------*

           READ CLIEADES.

           IF  WRK-FS-CLIEADES         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-CLIEADES.

           ADD 1                       TO ACU-LIDOS-CLIEADES.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL PARA CADA REGISTRO DO CLIESEG       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

RIC   *    PERFORM 3100-ACESSAR-GFCT5534.
RIC   *    PERFORM 3200-GRAVAR-CLIESEG.
RIC   *    PERFORM 2100-LER-CLIEADES.

RIC        PERFORM 3100-ACESSAR-GFCT5534.

RIC        ADD  1  TO  ACU-CLIE-PROCESSADO.
RIC
RIC        IF GFCTJM-COD-RETORNO = ZEROS
RIC           PERFORM 3200-GRAVAR-CLIESEG
RIC           PERFORM 2100-LER-CLIEADES
RIC        ELSE
RIC           ADD  1  TO  ACU-CLIE-NAO-CADASTRADO
RIC           PERFORM 2100-LER-CLIEADES
RIC        END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSAR MODULO GFCT5534                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-ACESSAR-GFCT5534           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA-RETORNO
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTJM-AREA-RETORNO
                      GFCT0M-AREA-ERROS.

           MOVE 'B'                    TO GFCTJM-TPO-AMBIENTE.
           MOVE GFCTRU-AGENCIA         TO GFCTJM-CJUNC-DEPDC.
           MOVE GFCTRU-CONTA           TO GFCTJM-CCTA-CLI.
           MOVE 'GFCT5534'             TO WRK-MODULO.

      *    CALL WRK-MODULO             USING GFCTJM-AREA
      *                                      GFCT0M-AREA-ERROS.

           CALL 'POOL0080'             USING WRK-MODULO
                                             GFCTJM-AREA
                                             GFCT0M-AREA-ERROS.

RIC   *    IF  RETURN-CODE             NOT EQUAL ZEROS OR
RIC   *        GFCTJM-COD-RETORNO      NOT EQUAL ZEROS
RIC   *        PERFORM 3110-EMITIR-DISPLAY-ERRO
RIC   *        PERFORM 4000-FINALIZAR
RIC   *    END-IF.

RIC        IF  RETURN-CODE               NOT EQUAL ZEROS OR
RIC            (GFCTJM-COD-RETORNO EQUAL 01 OR 02 OR 05 OR 07 OR
RIC                                      99)
RIC            PERFORM 3110-EMITIR-DISPLAY-ERRO
RIC            PERFORM 4000-FINALIZAR
RIC        END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMISSAO DE DISPLAY DE ERRO DE ACESSO AO MODULO GFCT5534     *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-EMITIR-DISPLAY-ERRO        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA         TO WRK-MASCARA-AGENCIA.
           MOVE GFCTRU-CONTA           TO WRK-MASCARA-CONTA.

           DISPLAY '*---------------------------*'.
           DISPLAY '* PROGRAMA GFCT2160         *'.
           DISPLAY '* ------------------------- *'.
           DISPLAY '* ERRO NO ACESSO AO MODULO  *'.
           DISPLAY '* GFCT5534 PELA AGENCIA E   *'.
           DISPLAY '* CONTA:                    *'.
           DISPLAY '* ' WRK-MASCARA-AGENCIA
                          '                     *'.
           DISPLAY '* ' WRK-MASCARA-CONTA
                            '                   *'.
           DISPLAY '*---------------------------*'.

           DISPLAY 'RETURN-CODE ' RETURN-CODE.
           DISPLAY 'GFCTJM-COD-RETORNO ' GFCTJM-COD-RETORNO.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVACAO DO ARQUIVO CLIESEG                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-GRAVAR-CLIESEG             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTRU-AGENCIA         TO GFCTRY-AGENCIA.
           MOVE GFCTRU-RAZAO           TO GFCTRY-RAZAO.
           MOVE GFCTRU-CONTA           TO GFCTRY-CONTA.
           MOVE GFCTRU-CGC-CPF         TO GFCTRY-CGC-CPF.
           MOVE GFCTRU-FILIAL          TO GFCTRY-FILIAL.
           MOVE GFCTRU-CONTR           TO GFCTRY-CONTR.
           MOVE GFCTRU-POSTO-SERV      TO GFCTRY-POSTO-SERV.
           MOVE GFCTRU-DT-ABERT        TO GFCTRY-DT-ABERT.
           MOVE GFCTJM-CSGMTO-GSTAO-TARIF
                                       TO GFCTRY-SEGTO-GFCT.
           MOVE ZEROS                  TO GFCTRY-MUNICIPIO.
           MOVE SPACES                 TO GFCTRY-SIGLA-UF.

           WRITE GFCTRY-ENTRADA.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-CLIESEG.

           ADD 1                       TO ACU-GRAVADOS-CLIESEG.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO          *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISP-FINAL.

           CLOSE CLIEADES
                 CLIESEG.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLAY FINAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISP-FINAL          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************* GFCT2160 *******************'.
           DISPLAY '*                                              *'.
           MOVE ACU-LIDOS-CLIEADES     TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG. LIDOS CLIEADES..: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-CLIE-PROCESSADO    TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG. PROCESSADOS.....: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-CLIE-NAO-CADASTRADO TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE NAO CADASTRADOS - GFCT '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           MOVE ACU-GRAVADOS-CLIESEG   TO WRK-MASCARA.
           DISPLAY '* TOTAIS DE REG. GRAVADOS CLIESEG: '
                   WRK-MASCARA                                   ' *'.
           DISPLAY '*                                              *'.
           DISPLAY '******************* GFCT2160 *******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2160'             TO ERR-PGM.
           MOVE 04                     TO RETURN-CODE.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
