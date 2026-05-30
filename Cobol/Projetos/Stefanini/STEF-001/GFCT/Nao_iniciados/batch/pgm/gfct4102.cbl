      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT4102.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4102                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: UBIRAJARA (BIRA)                         *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: JAN/2011                                 *
      *                                                                *
      *    PROJETO..........: PWT-10-3168 FICHA DE CADASTRO PJ         *
      *                                                                *
      *    OBJETIVO      :                                             *
      *      GERA MOVIMENTO PARA INCLUSAO DE PARCELAS GFCTB0I3 ,       *
      *      GERA MOVIMENTO DE ATUALIZACAO GFCTB092.                   *
      *                                                                *
      *    ARQUIVOS      :                                             *
      *      DDNAME                        INCLUDE/BOOK                *
      *      EVENTOSP                        I#GFCTVA  (INPUT )        *
      *      EVTPARCE                        GFCTWTOT  (INPUT )        *
      *      GFCTB0I3                        GFCTWTOY  (OUTPUT)        *
      *      GFCTB092                        I#GFCTVA  (OUTPUT)        *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - TRATAMENTO DE ERROS                            *
      *      POOL1640 - SOMAR DIAS, MESES E ANOS EM UMA DATA           *
      *      BRAD0025 - SOMA/SUBTRAI DIAS EM DETERMINADA DATA.         *
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
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EVENTOSP ASSIGN      TO UT-S-EVENTOSP
           FILE STATUS                 IS WRK-FS-EVENTOSP.

           SELECT EVTPARCE  ASSIGN     TO UT-S-EVTPARCE
           FILE STATUS                 IS WRK-FS-EVTPARCE.

           SELECT GFCTB0I3  ASSIGN     TO UT-S-GFCTB0I3
           FILE STATUS                 IS WRK-FS-GFCTB0I3.

           SELECT GFCTB092  ASSIGN     TO UT-S-GFCTB092
           FILE STATUS                 IS WRK-FS-GFCTB092.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  MOVIMENTO DE EVENTOS                                *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  EVENTOSP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
      *    INPUT:  EVENTOS PARCELADOS                                  *
      *            ORG. SEQUENCIAL     -   LRECL   =   265             *
      *----------------------------------------------------------------*

       FD  EVTPARCE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'GFCTWTOT'.

      *----------------------------------------------------------------*
      *   OUTPUT:  PARCELAS DE EVENTOS                                 *
      *            ORG. SEQUENCIAL     -   LRECL   =   073             *
      *----------------------------------------------------------------*

       FD  GFCTB0I3
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'GFCTWTOY'.

      *----------------------------------------------------------------*
      *   OUTPUT:  MOVIMENTO DE EVENTOS                                *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  GFCTB092
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-EVENTOSP          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-EVTPARCE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-GFCTB0I3           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-GFCTB092           PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-EVENTOSP             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-EVTPARCE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-GFCTB0I3             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-GFCTB092             PIC  X(002)         VALUE SPACES.

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
           '* VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MASCARA                 PIC  ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-MES-OK                  PIC S9(003)  COMP-3 VALUE ZEROS.
       77  WRK-ANO-OK                  PIC S9(005)  COMP-3 VALUE ZEROS.
       77  WRK-MES-E                   PIC S9(003)  COMP-3 VALUE ZEROS.
       77  WRK-MES-S                   PIC S9(003)  COMP-3 VALUE ZEROS.
       77  WRK-ANO-E                   PIC S9(005)  COMP-3 VALUE ZEROS.
       77  WRK-ANO-S                   PIC S9(005)  COMP-3 VALUE ZEROS.
       77  WRK-PARC                    PIC S9(003)  COMP-3 VALUE 1.
       77  WRK-TT-PARC-LIQ             PIC S9(9)V99 COMP-3 VALUE ZEROS.
       77  WRK-TT-PARC-CADTR           PIC S9(9)V99 COMP-3 VALUE ZEROS.
       77  WRK-TT-PARC-BRUTO           PIC S9(9)V99 COMP-3 VALUE ZEROS.
       77  WRK-DAGNDA-AUX              PIC  X(010)         VALUE SPACES.
       77  WRK-DAGNDA-CALCULADA        PIC  X(010)         VALUE SPACES.
       77  WRK-SIT-EVNTO-2             PIC S9(002)V COMP-3 VALUE +2.
       77  WRK-SIT-EVNTO-16            PIC S9(002)V COMP-3 VALUE +16.

       01  WRK-DATA-AUX-ENT            PIC  9(009)       VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-AUX-ENT.
           05 FILLER                   PIC  9(001).
           05 WRK-DIA-AUX-ENT          PIC  9(002).
           05 WRK-MES-AUX-ENT          PIC  9(002).
           05 WRK-ANO-AUX-ENT          PIC  9(004).

       01  WRK-DATA-AUX-SAI            PIC  9(009)       VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-AUX-SAI.
           05 FILLER                   PIC  9(001).
           05 WRK-DIA-AUX-SAI          PIC  9(002).
           05 WRK-MES-AUX-SAI          PIC  9(002).
           05 WRK-ANO-AUX-SAI          PIC  9(004).

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(001)       VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(001)       VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE MENSAGEM DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES DOS ARQUIVOS *'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-EVENTOSP.
           05 WRK-GFCTVA-CSERVC-TARIF  PIC  9(005)         VALUE ZEROS.
           05 WRK-GFCTVA-DOCOR-EVNTO   PIC  9(008)         VALUE ZEROS.

       01  WRK-CHAVE-EVTPARCE.
           05 WRK-TOT-CSERVC-TARIF     PIC  9(005)         VALUE ZEROS.
           05 WRK-TOT-DOCOR-EVNTO      PIC  9(008)         VALUE ZEROS.

       01  WRK-CHAVE-AUX               PIC  X(013)         VALUE SPACES.

       01  WRK-S9-2V                   PIC S9(02)V COMP-3 VALUE ZEROS.
       01  FILLER                      REDEFINES          WRK-S9-2V.
           05  WRK-S9-2                PIC S9(02)  COMP-3.

       01  WRK-S9-05                   PIC +9(05)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-S9-05.
           05 FILLER                   PIC  X(01).
           05 WRK-9-05                 PIC  9(05).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA POOL1640'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL1640.
           05  WRK-DT-ENTRADA-1640     PIC  X(010)         VALUE SPACES.
           05  WRK-FORMATO-ENT-1640    PIC  9(002)         VALUE ZEROS.
           05  WRK-TP-INCREM-1640      PIC  X(002)         VALUE SPACES.
           05  WRK-INCREMENTO-1640     PIC  9(003)         VALUE ZEROS.
           05  WRK-FORMATO-SAI-1640    PIC  9(002)         VALUE ZEROS.
           05  WRK-DT-SAIDA-1640       PIC  X(010)         VALUE SPACES.
           05  WRK-MENSAGEM-1640       PIC  X(080)         VALUE SPACES.

       01  WRK-INCREMENTO-1640-NC      PIC S9(003)         VALUE ZEROS.

       01  WRK-S9-03                   PIC +9(03)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-S9-03.
           05 FILLER                   PIC  X(01).
           05 WRK-9-03                 PIC  9(03).

       01  WRK-INCREMENTO-1640-COMP    PIC S9(003) COMP-3  VALUE ZEROS.

       01  WRK-MENSAGEM-1640-E.
           05  WRK-MENSAGEM-1640-R     PIC  X(040)         VALUE SPACES.
           05  FILLER                  PIC  X(040)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO A BRAD0025 ***'.
      *----------------------------------------------------------------*

       01  WRK-0025-DATA-ENTRADA  PIC  9(009) COMP-3 VALUE ZEROS.
       01  WRK-0025-DATA-SAIDA    PIC  9(009) COMP-3 VALUE ZEROS.
       01  WRK-0025-NUMERO-DIAS   PIC S9(005) COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-CHAVE-EVENTOSP  EQUAL HIGH-VALUES
                                       AND
                   WRK-CHAVE-EVTPARCE  EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  EVENTOSP
                       EVTPARCE
                OUTPUT GFCTB0I3
                       GFCTB092.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-EVENTOSP.

           PERFORM 1120-TESTAR-FS-EVTPARCE.

           PERFORM 1190-TESTAR-FS-GFCTB0I3.

           PERFORM 1191-TESTAR-FS-GFCTB092.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-EVENTOSP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EVENTOSP         NOT EQUAL '00'
               MOVE WRK-FS-EVENTOSP    TO WRK-FILE-STATUS
               MOVE 'EVENTOSP'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-EVTPARCE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EVTPARCE         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-EVTPARCE    TO WRK-FILE-STATUS
               MOVE 'EVTPARCE'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1190-TESTAR-FS-GFCTB0I3         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTB0I3         NOT EQUAL '00'
               MOVE WRK-FS-GFCTB0I3    TO WRK-FILE-STATUS
               MOVE 'GFCTB0I3'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1190-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1191-TESTAR-FS-GFCTB092         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-GFCTB092         NOT EQUAL '00'
               MOVE WRK-FS-GFCTB092    TO WRK-FILE-STATUS
               MOVE 'GFCTB092'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1191-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-EVTPARCE.

           IF  WRK-CHAVE-EVTPARCE      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT4102 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO EVTPARCE VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4102 *****************'
           END-IF.

           PERFORM 2200-LER-EVENTOSP.

           IF  WRK-CHAVE-EVENTOSP       EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT4102 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO EVENTOSP VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT4102 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-EVTPARCE               SECTION.
      *----------------------------------------------------------------*

           READ EVTPARCE.

           IF  WRK-FS-EVTPARCE         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EVTPARCE
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-EVTPARCE.

           MOVE   TOT-CSERVC-TARIF     TO   WRK-S9-05.
           MOVE   WRK-9-05             TO   WRK-TOT-CSERVC-TARIF.

           STRING TOT-DOCOR-EVNTO(07:04)
                  TOT-DOCOR-EVNTO(04:02)
                  TOT-DOCOR-EVNTO(01:02)
           DELIMITED BY SIZE           INTO WRK-TOT-DOCOR-EVNTO.

           ADD  1                      TO ACU-LIDOS-EVTPARCE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-EVENTOSP               SECTION.
      *----------------------------------------------------------------*

           READ EVENTOSP.

           IF  WRK-FS-EVENTOSP         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-EVENTOSP
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-EVENTOSP.

           MOVE   GFCTVA-CSERVC-TARIF  OF EVENTOSP
                                       TO   WRK-S9-05
           MOVE   WRK-9-05             TO   WRK-GFCTVA-CSERVC-TARIF.

           STRING GFCTVA-DOCOR-EVNTO OF EVENTOSP(07:04)
                  GFCTVA-DOCOR-EVNTO OF EVENTOSP(04:02)
                  GFCTVA-DOCOR-EVNTO OF EVENTOSP(01:02)
           DELIMITED BY SIZE           INTO WRK-GFCTVA-DOCOR-EVNTO.

           ADD 1                       TO   ACU-LIDOS-EVENTOSP.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO                 TO WRK-OPERACAO.

           IF  WRK-CHAVE-EVENTOSP            LESS WRK-CHAVE-EVTPARCE
               MOVE    GFCTVA-GFCTB092 OF EVENTOSP
                                       TO GFCTVA-GFCTB092 OF GFCTB092
               MOVE    'N' TO GFCTVA-CINDCD-TARIF-PCELD   OF GFCTB092
               WRITE   GFCTVA-GFCTB092 OF GFCTB092
               PERFORM 1191-TESTAR-FS-GFCTB092
               ADD     1               TO ACU-GRAV-GFCTB092
               PERFORM 2200-LER-EVENTOSP

           ELSE

               IF  WRK-CHAVE-EVENTOSP        GREATER WRK-CHAVE-EVTPARCE
                   IF  WRK-CHAVE-EVTPARCE  NOT EQUAL  WRK-CHAVE-AUX
                       DISPLAY 'OCORR. DE PARCELA NAO LOCALIZADA= '
                       WRK-CHAVE-EVTPARCE
                   END-IF
                   PERFORM 2100-LER-EVTPARCE

               ELSE

                   MOVE WRK-CHAVE-EVTPARCE   TO WRK-CHAVE-AUX
                   MOVE GFCTVA-GFCTB092 OF EVENTOSP
                                        TO GFCTVA-GFCTB092 OF GFCTB092
                   MOVE 'S' TO GFCTVA-CINDCD-TARIF-PCELD   OF GFCTB092

                   MOVE    ZEROS  TO     WRK-TT-PARC-LIQ
                                         WRK-TT-PARC-CADTR
                                         WRK-TT-PARC-BRUTO
                   PERFORM 3100-PROCESSA-GFCTB0I3

                   IF  WRK-DAGNDA-AUX   NOT EQUAL   SPACES
                       MOVE   WRK-DAGNDA-AUX  TO
                              GFCTVA-DAGNDA-DEB-MOVTO OF GFCTB092
                       MOVE   SPACES          TO      WRK-DAGNDA-AUX
                   END-IF

                   WRITE   GFCTVA-GFCTB092 OF GFCTB092
                   PERFORM 1191-TESTAR-FS-GFCTB092
                   ADD     1    TO ACU-GRAV-GFCTB092

                   PERFORM 2200-LER-EVENTOSP
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-PROCESSA-GFCTB0I3          SECTION.
      *----------------------------------------------------------------*

           PERFORM                     VARYING WRK-PARC FROM 1 BY 1
                                       UNTIL WRK-PARC  GREATER
                                             TOT-QPCELA-COBR-TARIF

               MOVE GFCTVA-CROTNA-ORIGE-MOVTO OF EVENTOSP
                           TO     TOY-CROTNA-ORIGE-MOVTO
               MOVE GFCTVA-DENVIO-MOVTO-TARIF OF EVENTOSP
                           TO     TOY-DENVIO-MOVTO-TARIF
               MOVE GFCTVA-CNRO-ARQ-MOVTO     OF EVENTOSP
                           TO            WRK-S9-2
               MOVE WRK-S9-2V     TO     TOY-CNRO-ARQ-MOVTO
               MOVE GFCTVA-CSEQ-MOVTO         OF EVENTOSP
                           TO     TOY-CSEQ-MOVTO
               MOVE WRK-PARC
                           TO     TOY-CPCELA-TARIF
               MOVE TOT-QPCELA-COBR-TARIF
                           TO     TOY-CTOT-PCELA-TARIF
               MOVE ZEROS
                           TO     TOY-VTARIF-DEB-MOVTO
               MOVE '01.01.0001'
                           TO     TOY-DEFETV-DEB-MOVTO
               COMPUTE     TOY-VTARIF-CADTR-SIST =
                           ( GFCTVA-VTARIF-CADTR-SIST OF EVENTOSP  /
                             TOT-QPCELA-COBR-TARIF )
               COMPUTE     TOY-VTARIF-BRUTO-TOT  =
                           ( GFCTVA-VTARIF-BRUTO-MOVTO  OF EVENTOSP  /
                             TOT-QPCELA-COBR-TARIF )

               IF   WRK-PARC  LESS  TOT-QPCELA-COBR-TARIF
                    COMPUTE  TOY-VTARIF-LIQ-MOVTO =
                             ( GFCTVA-VTARIF-LIQ-MOVTO  OF EVENTOSP  /
                               TOT-QPCELA-COBR-TARIF )
                    COMPUTE  WRK-TT-PARC-LIQ =
                             ( WRK-TT-PARC-LIQ + TOY-VTARIF-LIQ-MOVTO )
               ELSE
                    COMPUTE  TOY-VTARIF-LIQ-MOVTO =
                             ( GFCTVA-VTARIF-LIQ-MOVTO  OF EVENTOSP  -
                               WRK-TT-PARC-LIQ )
               END-IF
               IF   WRK-PARC  LESS  TOT-QPCELA-COBR-TARIF
                    COMPUTE  TOY-VTARIF-CADTR-SIST =
                             ( GFCTVA-VTARIF-CADTR-SIST OF EVENTOSP  /
                               TOT-QPCELA-COBR-TARIF )
                    COMPUTE  WRK-TT-PARC-CADTR =
                          ( WRK-TT-PARC-CADTR + TOY-VTARIF-CADTR-SIST )
               ELSE
                    COMPUTE  TOY-VTARIF-CADTR-SIST =
                             ( GFCTVA-VTARIF-CADTR-SIST  OF EVENTOSP  -
                               WRK-TT-PARC-CADTR )
               END-IF
               IF   WRK-PARC  LESS  TOT-QPCELA-COBR-TARIF
                    COMPUTE  TOY-VTARIF-BRUTO-TOT =
                             ( GFCTVA-VTARIF-BRUTO-MOVTO OF EVENTOSP  /
                               TOT-QPCELA-COBR-TARIF )
                    COMPUTE  WRK-TT-PARC-BRUTO =
                          ( WRK-TT-PARC-BRUTO + TOY-VTARIF-BRUTO-TOT )
               ELSE
                    COMPUTE  TOY-VTARIF-BRUTO-TOT =
                             ( GFCTVA-VTARIF-BRUTO-MOVTO OF EVENTOSP  -
                               WRK-TT-PARC-BRUTO )
               END-IF

               IF   TOY-VTARIF-LIQ-MOVTO   LESS   ZEROS
                    MOVE   ZEROS   TO   TOY-VTARIF-LIQ-MOVTO
                    MOVE  WRK-SIT-EVNTO-16
                                        TO   TOY-CSIT-EVNTO-RECBD
               ELSE
                    IF   TOY-VTARIF-LIQ-MOVTO   EQUAL   ZEROS
                         MOVE  WRK-SIT-EVNTO-16
                                        TO   TOY-CSIT-EVNTO-RECBD
                    ELSE
                         MOVE  WRK-SIT-EVNTO-2
                                        TO   TOY-CSIT-EVNTO-RECBD
                    END-IF
               END-IF

               PERFORM 3150-PROCESSO-AGENDAMENTO

               IF  TOY-CSIT-EVNTO-RECBD     EQUAL  WRK-SIT-EVNTO-16
                   MOVE GFCTVA-DAGNDA-DEB-MOVTO OF EVENTOSP
                                                TO TOY-DAGNDA-DEB-MOVTO
               ELSE
                   MOVE WRK-DAGNDA-CALCULADA    TO TOY-DAGNDA-DEB-MOVTO
               END-IF

               WRITE   GFCTWTOY-REG
               PERFORM 1190-TESTAR-FS-GFCTB0I3
               ADD     1    TO ACU-GRAV-GFCTB0I3

           END-PERFORM.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3150-PROCESSO-AGENDAMENTO       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-PARC  EQUAL  1
               MOVE GFCTVA-DAGNDA-DEB-MOVTO OF EVENTOSP
                           TO     TOY-DAGNDA-DEB-MOVTO
                                  WRK-DAGNDA-CALCULADA
               GO          TO     3150-01-SALVA-SIT-02
           END-IF.

           IF  TOT-CIDTFD-COBR-PCELA   EQUAL   'M'
               COMPUTE WRK-INCREMENTO-1640-COMP = ( WRK-PARC - 1 )
               MOVE WRK-INCREMENTO-1640-COMP TO   WRK-INCREMENTO-1640-NC
               MOVE WRK-INCREMENTO-1640-NC   TO   WRK-S9-03
               MOVE WRK-9-03                 TO   WRK-INCREMENTO-1640
               MOVE 'M'                      TO   WRK-TP-INCREM-1640
               MOVE GFCTVA-DAGNDA-DEB-MOVTO OF EVENTOSP
                                             TO   WRK-DAGNDA-CALCULADA
               PERFORM     3200-CALCULAR-DATA-AGMTO
               MOVE        ZEROS             TO   WRK-MES-OK
               PERFORM     3300-AJUSTA-MES
           ELSE
               MOVE TOT-QDIA-COBR-PCELA      TO   WRK-INCREMENTO-1640-NC
               MOVE WRK-INCREMENTO-1640-NC   TO   WRK-S9-03
               MOVE WRK-9-03                 TO   WRK-INCREMENTO-1640
               MOVE 'DC'                     TO   WRK-TP-INCREM-1640
               PERFORM 3200-CALCULAR-DATA-AGMTO
           END-IF.

      ** MOVIMENTA DATA DE AGENDAMENTO PARA SITUACAO=02
       3150-01-SALVA-SIT-02.

           IF  (TOY-CSIT-EVNTO-RECBD     EQUAL  WRK-SIT-EVNTO-2) AND
               (WRK-DAGNDA-AUX           EQUAL  SPACES)
               IF  WRK-PARC  EQUAL  1
                   MOVE GFCTVA-DAGNDA-DEB-MOVTO OF EVENTOSP
                                                TO WRK-DAGNDA-AUX
           ELSE
                   MOVE WRK-DAGNDA-CALCULADA    TO WRK-DAGNDA-AUX
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-CALCULAR-DATA-AGMTO        SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-DAGNDA-CALCULADA    TO WRK-DT-ENTRADA-1640.

           MOVE 09                      TO WRK-FORMATO-ENT-1640
                                           WRK-FORMATO-SAI-1640.

           CALL 'POOL1640'           USING WRK-DT-ENTRADA-1640
                                           WRK-FORMATO-ENT-1640
                                           WRK-TP-INCREM-1640
                                           WRK-INCREMENTO-1640
                                           WRK-FORMATO-SAI-1640
                                           WRK-DT-SAIDA-1640
                                           WRK-MENSAGEM-1640.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'                  TO ERR-TIPO-ACESSO
               MOVE WRK-MENSAGEM-1640      TO WRK-MENSAGEM-1640-E
               MOVE WRK-MENSAGEM-1640-R    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DT-SAIDA-1640      TO   WRK-DAGNDA-CALCULADA
                                            GFCTVA-DAGNDA-DEB-MOVTO
                                            OF GFCTB092.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-AJUSTA-MES                 SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-DT-ENTRADA-1640      TO   WRK-DATA-DB2.
           MOVE WRK-MES-DB2              TO   WRK-MES-AUX-ENT.
           MOVE WRK-ANO-DB2              TO   WRK-ANO-AUX-ENT.

           MOVE WRK-DT-SAIDA-1640        TO   WRK-DATA-DB2.
           MOVE WRK-DIA-DB2              TO   WRK-DIA-AUX-SAI.
           MOVE WRK-MES-DB2              TO   WRK-MES-AUX-SAI.
           MOVE WRK-ANO-DB2              TO   WRK-ANO-AUX-SAI.
           MOVE WRK-DATA-AUX-SAI         TO   WRK-0025-DATA-ENTRADA.

           MOVE  WRK-MES-AUX-ENT         TO   WRK-MES-E.
           MOVE  WRK-MES-AUX-SAI         TO   WRK-MES-S.
           MOVE  WRK-ANO-AUX-ENT         TO   WRK-ANO-E.
           MOVE  WRK-ANO-AUX-SAI         TO   WRK-ANO-S.

           COMPUTE  WRK-ANO-OK  =  (WRK-ANO-S  -  WRK-ANO-E)

           IF   WRK-MES-E   GREATER   WRK-MES-S
                COMPUTE  WRK-MES-S  =  (WRK-MES-S + (WRK-ANO-OK * 12))
           END-IF.

           IF   WRK-MES-E  EQUAL    WRK-MES-S
                COMPUTE  WRK-MES-OK =  (WRK-MES-OK + (WRK-ANO-OK * 12))
           ELSE
                COMPUTE  WRK-MES-OK =  (WRK-MES-S - WRK-MES-E)
                IF   WRK-INCREMENTO-1640-COMP  GREATER  WRK-MES-OK
                     COMPUTE  WRK-MES-OK =
                              (WRK-MES-OK + (WRK-ANO-OK * 12))
                END-IF
           END-IF.

           IF   WRK-MES-OK    NOT EQUAL   WRK-INCREMENTO-1640-COMP
                PERFORM       3400-CHAMA-BRAD0025
                MOVE   WRK-0025-DATA-SAIDA   TO  WRK-DATA-AUX-SAI
                MOVE   WRK-DIA-AUX-SAI       TO  WRK-DIA-DB2
                MOVE   WRK-MES-AUX-SAI       TO  WRK-MES-DB2
                MOVE   WRK-ANO-AUX-SAI       TO  WRK-ANO-DB2
                MOVE   WRK-DATA-DB2          TO  WRK-DT-SAIDA-1640
                GO     TO      3300-AJUSTA-MES
           END-IF.

           MOVE WRK-DT-SAIDA-1640      TO   WRK-DAGNDA-CALCULADA
                                            GFCTVA-DAGNDA-DEB-MOVTO
                                            OF GFCTB092.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3400-CHAMA-BRAD0025             SECTION.
      *---------------------------------------------------------------*

           MOVE -1                 TO    WRK-0025-NUMERO-DIAS.

           CALL 'BRAD0025'         USING WRK-0025-DATA-ENTRADA
                                         WRK-0025-NUMERO-DIAS
                                         WRK-0025-DATA-SAIDA.

           IF RETURN-CODE          NOT EQUAL ZEROS
              MOVE    'APL'                 TO ERR-TIPO-ACESSO
              MOVE    'ERRO NA BRAD0025'    TO ERR-TEXTO
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE EVTPARCE
                 EVENTOSP
                 GFCTB0I3
                 GFCTB092.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT4102 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-EVTPARCE     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EM EVTPARCE...: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-LIDOS-EVENTOSP     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EM EVENTOSP...: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-GRAV-GFCTB0I3      TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS EM GFCTB0I3: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-GRAV-GFCTB092      TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS EM GFCTB092: ' WRK-MASCARA
                                                                  ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT4102 ********************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 4                      TO RETURN-CODE.
           MOVE 'GFCT4102'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
