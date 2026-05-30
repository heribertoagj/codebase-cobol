      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT4780.
       AUTHOR.     SONDA.
      *================================================================*
      *                        SONDA IT                                *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4780                                     *
      *    PROGRAMADOR.:  SONDA                   - SONDA IT           *
      *    GERENTE RESP:  RODRIGO                 - SONDA IT           *
      *    DATA........:  02/08/2016.                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  FAZ BALANCE-LINE ENTRE O ARQUIVO CTAS HSBC   *
      *                   OBTER APENAS TARIFAS MONITORADAS PARA EMISSAO*
      *                   DE RELATORIO - DEVOLVE ARQ ISD PARA GESTOR   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  SCCEHSBC                    GFCTWF70          *
      *                  MOVTOISD                    GFCTWF71          *
      *                  GFCTEVTO                    I#GFCTVA          *
      *                  ARQSAIDA                    GFCTWF72          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    ENTRADAS:                                                   *
      *    GFCTWF70 - ARQUIVO SCCE COM CONTAS HSBC                     *
      *    GFCTWF71 - ARQUIVO ENVIADO P/ GESTOR TARIFAS MONITORADAS    *
      *    I#GFCTVA - ARQUIVO OBTIDO DO ARQUIVO DE MVTO BRADESCO       *
      *    SAIDA:                                                      *
      *    GFCTWF72 - ARQUIVO DE RETORNO COM VALORES COBRADOS          *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
      *    BRAD0488 - MANIPULACAO DE ARQUIVOS                          *
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

           SELECT SCCEHSBC  ASSIGN      TO UT-S-SCCEHSBC
           FILE STATUS                  IS WRK-FS-SCCEHSBC.

           SELECT MOVTOISD  ASSIGN      TO UT-S-MOVTOISD
           FILE STATUS                  IS WRK-FS-MOVTOISD.

           SELECT GFCTEVTO  ASSIGN      TO UT-S-GFCTEVTO
           FILE STATUS                  IS WRK-FS-GFCTEVTO.

           SELECT ARQSAIDA  ASSIGN      TO UT-S-ARQSAIDA
           FILE STATUS                  IS WRK-FS-ARQSAIDA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *   INPUT :  ARQUIVO DE ENTRADA  -   SCCEHSBC                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   014             *
      *----------------------------------------------------------------*

       FD  SCCEHSBC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-SCCEHSBC                  PIC  X(014).

      *----------------------------------------------------------------*
      *   INPUT :  ARQUIVO DE ENTRADA  -   MOVTOISD                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   017             *
      *----------------------------------------------------------------*

       FD  MOVTOISD
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-MOVTOISD                  PIC  X(017).

      *----------------------------------------------------------------*
      *   INPUT :  ARQUIVO DE ENTRADA  -   GFCTEVTO                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  GFCTEVTO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01  FD-GFCTEVTO                  PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   ARQSAIDA                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   055             *
      *----------------------------------------------------------------*

       FD  ARQSAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-ARQSAIDA                  PIC  X(055).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-SCCEHSBC          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-MOVTOISD          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-GFCTEVTO          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ARQSAIDA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-PJ-SCCEHSBC             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-COM-CORRESP             PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  ZZZZZZZ.ZZZ.ZZ9
                                                           VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-SCCEHSBC             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-MOVTOISD             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-GFCTEVTO             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQSAIDA             PIC  X(002)         VALUE SPACES.

       01  WRK-MONITORA                PIC  X(001)         VALUE 'N'.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

       01  WRK-AREAS.

        03 WRK-CHAVE-SCCEHSBC            PIC  9(018)  VALUE ZEROS.
        03 WRK-CHAVE-SCCEHSBC-R    REDEFINES WRK-CHAVE-SCCEHSBC.
           05  WRK-AGENCIA-HSBC          PIC  9(005).
           05  WRK-CONTA-HSBC            PIC  9(013).

        03 WRK-CHAVE-GFCTEVTO            PIC  9(018)  VALUE ZEROS.
        03 WRK-CHAVE-GFCTEVTO-R    REDEFINES  WRK-CHAVE-GFCTEVTO.
           05  WRK-AGENCIA-MOVT          PIC  9(005).
           05  WRK-CONTA-MOVT            PIC  9(013).

        03 WRK-CAMPO-TIRA-SINAL-13     PIC +9(013)         VALUE ZEROS.
        03 FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL-13.
           05  FILLER                  PIC  X.
           05  WRK-CAMPO-SEM-SINAL-13  PIC  9(13).

        03 WRK-CAMPO-TIRA-SINAL-5      PIC +9(005)         VALUE ZEROS.
        03 FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL-5.
           05  FILLER                  PIC X.
           05  WRK-CAMPO-SEM-SINAL-5   PIC 9(05).

        03 WRK-CAMPO-TIRA-S-09V2       PIC +9(009)V99      VALUE ZEROS.
        03 FILLER  REDEFINES           WRK-CAMPO-TIRA-S-09V2.
           05  FILLER                  PIC X.
           05  WRK-CAMPO-SEM-S-09V2    PIC 9(09)V99.

        03 WRK-RETURN-CODE             PIC +9(04)      VALUE ZEROS.
        03 FILLER                      REDEFINES       WRK-RETURN-CODE.
           05  FILLER                  PIC  X(01).
           05  WRK-RETURN-CODE-N       PIC  9(04).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA PARA A BRAD0488 - MANIP. DE ARQUIVOS ***'.
      *---------------------------------------------------------------*

       01  WRK-CAMPOS.
           05  WRK-CODIGO1             PIC  9(01)  VALUE 1.
           05  WRK-CODIGO2             PIC  9(01)  VALUE 2.
           05  WRK-CODIGO3             PIC  9(01)  VALUE 3.
           05  WRK-NOME-TABELA         PIC  X(08)  VALUE 'TARIFAS'.
           05  WRK-TAM-CHAVE           PIC  9(02)  VALUE 05.
           05  WRK-TAM-OCORR           PIC  9(03)  VALUE 20.

       01  WRK-OCORRENCIA.
           05  WRK-CHV-TARIFA          PIC  9(05)    VALUE ZEROS.
           05  WRK-VAL-TARIFA          PIC  9(09)V99 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05 FILLER                   PIC  X(009)         VALUE
              '*** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DOS ARQUIVOS'.
      *----------------------------------------------------------------*
      *SCCEHSBC--------------------------------------------------------*

       COPY 'GFCTWF70'.

      *MOVTOISD--------------------------------------------------------*

       COPY 'GFCTWF71'.

      *GFCTEVTO--------------------------------------------------------*

       COPY 'I#GFCTVA'.

      *ARQSAIDA--------------------------------------------------------*

       COPY 'GFCTWF72'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR UNTIL
                   WRK-CHAVE-GFCTEVTO-R EQUAL HIGH-VALUES
               AND WRK-CHAVE-SCCEHSBC-R EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DOS ARQUIVOS E TESTE DE FILE-STATUS.               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  SCCEHSBC
                       MOVTOISD
                       GFCTEVTO
                OUTPUT ARQSAIDA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DOS ARQUIVOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-SCCEHSBC.
           PERFORM 1120-TESTAR-FS-MOVTOISD.
           PERFORM 1130-TESTAR-FS-GFCTEVTO.
           PERFORM 1140-TESTAR-FS-ARQSAIDA.


      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO SCCEHSBC.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-SCCEHSBC         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SCCEHSBC         NOT EQUAL '00'
               MOVE 'SCCEHSBC'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-SCCEHSBC   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO MOVTOISD.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-MOVTOISD         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MOVTOISD         NOT EQUAL '00'
               MOVE 'MOVTOISD'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-MOVTOISD   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO GFCTEVTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-GFCTEVTO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTEVTO         NOT EQUAL '00'
               MOVE 'GFCTEVTO'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-GFCTEVTO   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-ARQSAIDA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQSAIDA         NOT EQUAL '00'
               MOVE 'ARQSAIDA'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-ARQSAIDA   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS.           *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-SCCEHSBC.

           IF  WRK-FS-SCCEHSBC         EQUAL '10'
               DISPLAY '***************** GFCT4780 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO SCCEHSBC VAZIO           *'
               DISPLAY '*         PROGRAMA SERA CANCELADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4780 *****************'
               MOVE  'SCCEHSBC'        TO  WRK-NOME-ARQ
               MOVE  WRK-FS-SCCEHSBC   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2200-LER-GFCTEVTO.

           IF  WRK-FS-GFCTEVTO         EQUAL '10'
               DISPLAY '***************** GFCT4780 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO GFCTEVTO VAZIO           *'
               DISPLAY '*        PROCESSAMENTO  CONTINUA           *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4780 *****************'
           END-IF.

           PERFORM 2300-LER-MOVTOISD.

           IF  WRK-FS-MOVTOISD         EQUAL '10'
               DISPLAY '***************** GFCT4780 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*         ARQUIVO MOVTOISD VAZIO           *'
               DISPLAY '*         PROGRAMA SERA CANCELADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4780 *****************'
               MOVE  'MOVTOISD'        TO  WRK-NOME-ARQ
               MOVE  WRK-FS-MOVTOISD   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2400-CARREGAR-TARIFAS  UNTIL
                   WRK-FS-MOVTOISD        EQUAL '10'.

           PERFORM 2500-FECHAR-TARIFAS.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO SCCEHSBC.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-SCCEHSBC               SECTION.
      *----------------------------------------------------------------*

           READ SCCEHSBC               INTO REG-MOVTO-F70.

           IF  WRK-FS-SCCEHSBC         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-SCCEHSBC-R
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1110-TESTAR-FS-SCCEHSBC.

           MOVE F70-E-CAG-HSBC         TO WRK-AGENCIA-HSBC.
           MOVE F70-E-CCTA-HSBC        TO WRK-CONTA-HSBC.

           ADD 1                       TO ACU-LIDOS-SCCEHSBC.

           IF  F70-E-TPSSOA            EQUAL 'PJ'
               ADD 1                   TO ACU-PJ-SCCEHSBC
               GO                      TO 2100-LER-SCCEHSBC
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO GFCTEVTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-GFCTEVTO               SECTION.
      *----------------------------------------------------------------*

           READ GFCTEVTO               INTO   GFCTVA-GFCTB092.

           IF  WRK-FS-GFCTEVTO         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-GFCTEVTO-R
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1130-TESTAR-FS-GFCTEVTO.

           MOVE GFCTVA-CAG-DSTNO-MOVTO  TO WRK-CAMPO-TIRA-SINAL-5.
           MOVE WRK-CAMPO-SEM-SINAL-5   TO WRK-AGENCIA-MOVT.

           MOVE GFCTVA-CCTA-DSTNO-MOVTO TO WRK-CAMPO-TIRA-SINAL-13.
           MOVE WRK-CAMPO-SEM-SINAL-13  TO WRK-CONTA-MOVT.

           ADD 1                        TO ACU-LIDOS-GFCTEVTO.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO GFCTEVTO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-LER-MOVTOISD               SECTION.
      *----------------------------------------------------------------*

           READ MOVTOISD               INTO   F71-MONIT-HSBC-REG.

           IF  WRK-FS-MOVTOISD         EQUAL '10'
               GO                      TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-MOVTOISD.

           ADD 1                       TO ACU-LIDOS-MOVTOISD.

           IF  F71-D-TP-REG-R          NOT EQUAL 1
               GO                      TO 2300-LER-MOVTOISD
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CARREGA TABELA INTERNA COM TARIFAS ENVIADAS PELO GESTOR     *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CARREGAR-TARIFAS           SECTION.
      *----------------------------------------------------------------*

           MOVE F71-D-TARIF-R          TO WRK-CHV-TARIFA
           MOVE F71-D-VTARIF-R         TO WRK-VAL-TARIFA

           CALL 'BRAD0488'             USING WRK-CODIGO1
                                             WRK-NOME-TABELA
                                             WRK-TAM-CHAVE
                                             WRK-TAM-OCORR
                                             WRK-OCORRENCIA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               DISPLAY '****************** GFCT4780 ******************'
               DISPLAY '*        ERRO NA BRAD0488 NA GRAVACAO        *'
               DISPLAY '*     DA TABELA TARIFAS RETURN-CODE = '
                              WRK-RETURN-CODE-N                  '   *'
               DISPLAY '**********************************************'
               DISPLAY '* >>> OCORRENCIA....: ' WRK-OCORRENCIA
               DISPLAY '* >>> LIDOS MOVTOISD: ' ACU-LIDOS-MOVTOISD
               DISPLAY '****************** GFCT4780 ******************'
               MOVE '*** ERRO NA BRAD0488 NA GRAVACAO DA TABELA ***'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2300-LER-MOVTOISD.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHA   TABELA INTERNA COM TARIFAS ENVIADAS PELO GESTOR     *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-FECHAR-TARIFAS             SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0488'             USING WRK-CODIGO2
                                             WRK-NOME-TABELA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               DISPLAY '****************** GFCT4780 ******************'
               DISPLAY '*        ERRO NA BRAD0488 NO FECHAMENTO      *'
               DISPLAY '*     DA TABELA TARIFAS RETURN-CODE = '
                              WRK-RETURN-CODE-N                  '   *'
               DISPLAY '****************** GFCT4780 ******************'
               MOVE '*** ERRO NA BRAD0488 NO FECHAMENTO DA TABELA ***'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-GFCTEVTO      EQUAL  WRK-CHAVE-SCCEHSBC
               PERFORM 3100-GERA-ARQSAIDA
               PERFORM 2200-LER-GFCTEVTO
               ADD 1                   TO ACU-COM-CORRESP
           ELSE
               IF  WRK-CHAVE-GFCTEVTO  LESS   WRK-CHAVE-SCCEHSBC
                   PERFORM 2200-LER-GFCTEVTO
               ELSE
                   PERFORM 2100-LER-SCCEHSBC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GERAR  SAIDA COM CONTAS DE MIGRA¸¶O (BRAD - HSBC)
      ******************************************************************
      *----------------------------------------------------------------*
       3100-GERA-ARQSAIDA                SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                      TO WRK-MONITORA

           PERFORM 3200-PESQUISA-TARIFA

           IF   WRK-MONITORA             EQUAL  'S'
                PERFORM 3300-GRAVA-ARQSAIDA
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PESQUISA TABELA INTERNA DE TARIFAS ENVIADAS                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PESQUISA-TARIFA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSERVC-TARIF    TO WRK-CAMPO-TIRA-SINAL-5.
           MOVE WRK-CAMPO-SEM-SINAL-5  TO WRK-CHV-TARIFA.

           CALL 'BRAD0488'             USING WRK-CODIGO3
                                             WRK-NOME-TABELA
                                             WRK-CHV-TARIFA
                                             WRK-OCORRENCIA.

           EVALUATE RETURN-CODE

               WHEN ZEROS
                    MOVE 'S'           TO WRK-MONITORA

               WHEN OTHER
                    MOVE 'N'           TO WRK-MONITORA

           END-EVALUATE.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR SAIDA COM CONTAS DE MIGRA¸¶O (BRAD - HSBC)
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVA-ARQSAIDA               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-MOVT         TO F72-D-AGEN-R
           MOVE WRK-CONTA-MOVT           TO F72-D-CTA-R
           MOVE WRK-CHV-TARIFA           TO F72-D-TARIF-R
           MOVE GFCTVA-DOCOR-EVNTO       TO F72-D-DT-EVENTO-R
           MOVE GFCTVA-VTARIF-LIQ-MOVTO  TO WRK-CAMPO-TIRA-S-09V2.
           MOVE WRK-CAMPO-SEM-S-09V2     TO F72-D-VCOBR-R
           MOVE WRK-VAL-TARIFA           TO F72-D-VHSBC-R

           MOVE WRK-GRAVACAO             TO  WRK-OPERACAO.
           WRITE FD-ARQSAIDA             FROM F72-MONIT-HSBC-REG.

           ADD 1                         TO  ACU-GRAVA-ARQSAIDA.

           PERFORM 1140-TESTAR-FS-ARQSAIDA.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE SCCEHSBC
                 GFCTEVTO
                 MOVTOISD
                 ARQSAIDA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMISSAO DE TOTAIS DE LIDOS E GRAVADOS.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*--------------------------------------------*'.
           DISPLAY '*              PROGRAMA GFCT4780             *'.
           DISPLAY '*              -----------------             *'.
           DISPLAY '*                                            *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO     *'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-SCCEHSBC     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS  SCCEHSBC   = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-GFCTEVTO     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS  GFCTEVTO   = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-LIDOS-MOVTOISD     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS  MOVTOISD   = ' WRK-MASCARA   ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-GRAVA-ARQSAIDA     TO WRK-MASCARA.
           DISPLAY '* REGS GRAVADOS ARQSAIDA = ' WRK-MASCARA    ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-PJ-SCCEHSBC        TO WRK-MASCARA.
           DISPLAY '* PJ-ENVIADAS PELO SCCE  = ' WRK-MASCARA    ' *'.
           DISPLAY '*                                            *'.

           MOVE ACU-COM-CORRESP        TO WRK-MASCARA.
           DISPLAY '* EVENTOS C/CORRESP HSBC = ' WRK-MASCARA    ' *'.
           DISPLAY '*                                            *'.
           DISPLAY '*--------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'GFCT4780'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
