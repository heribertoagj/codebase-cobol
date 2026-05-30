      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE6450.
       AUTHOR.     DANILO AUGUSTO MOREIRA.
      *================================================================*
      *            C P M  B R A X I S  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....:     BVVE6450                                  *
      *    PROGRAMADOR.:     DANILO AUGUSTO MOREIRA  - CPMBRAXIS       *
      *    ANALISTA    :     DANILO AUGUSTO MOREIRA  - CPMBRAXIS       *
      *    DATA........:     08/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:     LISTA CONFIRMACOES DOMICILIO COM ERRO DE  *
      *                      CONSISTENCIA FISICA                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME                         INCLUDE/BOOK               *
      *           CDOMERRO                    I#BVVEH0                 *
      *----------------------------------------------------------------*
      *    INC'S/BOOK'S:                                               *
      *      I#BVVEG8                                                  *
      *----------------------------------------------------------------*
      *    MODULOS.....:                    INCLUDE/BOOK               *
      *      BRAD7100                         I#BRAD7C                 *
      *      POOL7600                                                  *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

             SELECT CDOMERRO ASSIGN    TO UT-S-CDOMERRO
                        FILE STATUS    IS WRK-FS-CDOMERRO.

             SELECT RELATO   ASSIGN    TO UT-S-RELATO
                        FILE STATUS    IS WRK-FS-RELATO.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * INPUT                                                          *
      *----------------------------------------------------------------*

       FD  CDOMERRO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEH0'.

      *----------------------------------------------------------------*
      * OUTPUT                                                         *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  LINHA-RELATO                PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  ACU-PAGINAS                 PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(003) COMP-3  VALUE 55.
       77  ACU-GRAVADOS                PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-PROPOSTAS               PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS                   PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-DESCOMPACTA             PIC  9(015)         VALUE ZEROS.
       77  WRK-IND-ERROS               PIC  9(003) COMP-3  VALUE ZEROS.

           COPY 'I#BVVEH4'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES E FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FIM-CDOMERRO            PIC  X(001)         VALUE SPACES.

       01  WRK-FS-ARQUIVOS.
           05  WRK-FS-CDOMERRO         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-RELATO           PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.
       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RELATORIO ***'.
      *----------------------------------------------------------------*


       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(057)         VALUE
               'BVVE6450'.
           03  FILLER                  PIC  X(064)         VALUE
               'BANCO BRADESCO S/A'.
           03  FILLER                  PIC  X(006)         VALUE
               'PAG.: '.
           03  CB1-PAG                 PIC  ZZZZ9.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  CB2-DT-DD               PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-DT-MM               PIC  99.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-DT-AAAA             PIC  9999.
           03  FILLER                  PIC  X(042)         VALUE SPACES.
           03  FILLER                  PIC  X(072)         VALUE
               'CARTAO DE CREDITO - VENDEDOR'.
           03  CB2-TI-HH               PIC  99.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB2-TI-MM               PIC  99.
           03  FILLER                  PIC  X(001)         VALUE ':'.
           03  CB2-TI-SS               PIC  99.

       01  CABEC3.
           03  FILLER                  PIC  X(133)         VALUE SPACES.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'RELACAO DE REGISTROS COM ERRO DE CONSISTENCIA FISICA -'.
           03  FILLER                  PIC  X(044)         VALUE
               ' ARQUIVO DE CONFIRMACOES DE DOMILIO BANCARIO'.

       01  CABEC5.
           03  FILLER                  PIC  X(133)         VALUE SPACES.

       01  CABEC6.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(008)         VALUE
               'TIPREG'.
           03  FILLER                  PIC  X(008)         VALUE
               'BCOENV'.
           03  FILLER                  PIC  X(010)         VALUE
               'DATAENV'.
           03  FILLER                  PIC  X(009)         VALUE
               'HORAENV'.
           03  FILLER                  PIC  X(010)         VALUE
               'DATACTRL'.
           03  FILLER                  PIC  X(012)         VALUE
               'VERSAOARQ'.
           03  FILLER                  PIC  X(011)         VALUE
               '!..PV...!'.
           03  FILLER                  PIC  X(016)         VALUE
               'CNPJ/CPF ESTAB'.
           03  FILLER                  PIC  X(005)         VALUE 'F/J'.
           03  FILLER                  PIC  X(007)         VALUE
               'AGEN.'.
           03  FILLER                  PIC  X(016)         VALUE
               '!..CONTA-DIG...!'.

       01  CABEC7.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(067)         VALUE SPACES.
           03  FILLER                  PIC  X(045)         VALUE
               '!.....DESCRICAO DOS ERROS IDENTIFICADOS.....!'.

       01  CABEC8.
           03  FILLER                  PIC  X(133)         VALUE SPACES.

       01  CABEC9.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(112)         VALUE ALL'-'.

       01  LINDET1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  LD1-TP-REG              PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  LD1-BCO-ENV             PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  LD1-DT-ENV              PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-HORA-ENV            PIC  X(006)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-DT-CTRL             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-VERSAO-ARQ          PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-PV                  PIC  X(009)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-CNPJ-CPF-ESTBL      PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-F-J                 PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  LD1-CAGEN               PIC  X(005)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  LD1-CCTA                PIC  X(014)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '-'.
           03  LD1-CCTA-DIG            PIC  X(001)         VALUE SPACES.

       01  LINDET2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(067)         VALUE SPACES.
           03  LD2-DESC-ERRO           PIC  X(045)         VALUE SPACES.

       01  CABEC10.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(112)         VALUE ALL'-'.

       01  CABEC11.
           03  FILLER                  PIC  X(133)         VALUE SPACES.

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'TOTAL DE REGISTROS INCONSISTENTES: '.
           03  LT1-TTL-REG-INCONSIS    PIC  ZZ.ZZZ.ZZZ.ZZ9.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO DO FILE STATUS ***'.
      *----------------------------------------------------------------*

       01 WRK-ERRO-TEXTO.
          05  FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
          05  WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
          05  FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
          05  WRK-ARQUIVO              PIC  X(008)         VALUE SPACES.
          05  FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
          05  WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
          05  FILLER                   PIC  X(003)         VALUE ' **'.
          05  FILLER                   PIC  X(012)         VALUE SPACES.

          COPY 'I#BRAD7C'.
          05  WRK-ERR-DADOS-SENHAS.
              10  WRK-ERR-COD-USER    PIC  X(007)         VALUE SPACES.
              10  WRK-ERR-COD-DEPTO   PIC  X(006)         VALUE SPACES.
              10  WRK-ERR-MODULO      PIC  X(008)         VALUE SPACES.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC  9(005)  COMP-3 VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC  9(007)  COMP-3 VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC  9(009)  COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC  9(007)  COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSSMMMMMM     PIC  9(013)  COMP-3 VALUE ZEROS.
           05  TIMESTAMP               PIC  X(020)         VALUE SPACES.


       01  WRK-DATA-AAAAMMDD-RDF       PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-DATA-AAAAMMDD-RDF.
           05  WRK-DT-AAAA             PIC  9(004).
           05  WRK-DT-MM               PIC  9(002).
           05  WRK-DT-DD               PIC  9(002).

       01  WRK-DATA-DDMMAAAA-RDF.
           05  WRK-DT-R-DD             PIC  9(002)         VALUE ZEROS.
           05  WRK-DT-R-MM             PIC  9(002)         VALUE ZEROS.
           05  WRK-DT-R-AAAA           PIC  9(004)         VALUE ZEROS.

       01  WRK-TI-HHMMSS-RDF           PIC  9(006)         VALUE ZEROS.
       01  FILLER REDEFINES  WRK-TI-HHMMSS-RDF.
           05  WRK-TI-HH               PIC  9(002).
           05  WRK-TI-MM               PIC  9(002).
           05  WRK-TI-SS               PIC  9(002).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*


      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZACAO.

           PERFORM 2000-PROCESSAR
             UNTIL WRK-FIM-CDOMERRO    EQUAL  'S'.
           PERFORM 3000-FINALIZACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZACAO              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  CDOMERRO
                OUTPUT RELATO.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO
           PERFORM 1110-TESTAR-FS-CDOMERRO.
           CALL  'POOL7600'  USING     WRK-DATA-HORA.
           MOVE WRK-DT-AAAAMMDD        TO WRK-DATA-AAAAMMDD-RDF.
           MOVE WRK-TI-HHMMSS          TO WRK-TI-HHMMSS-RDF.
           PERFORM 1300-VERIFICA-VAZIO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-CDOMERRO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CDOMERRO         NOT EQUAL '00'
               MOVE WRK-FS-CDOMERRO    TO  WRK-FILE-STATUS
               MOVE 'CDOMERRO'         TO  WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO  ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE 'RELATO  '         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-LER-CDOMERRO.

           IF  WRK-FIM-CDOMERRO        EQUAL 'S'
               DISPLAY '*********** BVVE6450 ************'
               DISPLAY '*                               *'
               DISPLAY '*  ARQUIVO CDOMERRO ESTA VAZIO  *'
               DISPLAY '*                               *'
               DISPLAY '*********** BVVE6450 ************'
               PERFORM 2100-EMITIR-CABEC
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-LER-CDOMERRO               SECTION.
      *----------------------------------------------------------------*

           READ CDOMERRO
             AT END
                MOVE 'S'               TO WRK-FIM-CDOMERRO
            NOT AT END
                MOVE WRK-LEITURA       TO WRK-OPERACAO
                PERFORM 1110-TESTAR-FS-CDOMERRO
                ADD  1                 TO ACU-LIDOS
           END-READ.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS      NOT GREATER 52
               PERFORM 2200-EMITIR-REG
               PERFORM 1400-LER-CDOMERRO
           ELSE
               PERFORM 2100-EMITIR-CABEC
               PERFORM 2200-EMITIR-REG
               PERFORM 1400-LER-CDOMERRO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-EMITIR-CABEC               SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.
           MOVE ACU-PAGINAS            TO WRK-DESCOMPACTA.
           MOVE WRK-DESCOMPACTA        TO CB1-PAG.
           MOVE CABEC1                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE WRK-DT-AAAAMMDD        TO WRK-DATA-AAAAMMDD-RDF.

           MOVE WRK-DT-DD              TO CB2-DT-DD.
           MOVE WRK-DT-MM              TO CB2-DT-MM.
           MOVE WRK-DT-AAAA            TO CB2-DT-AAAA.

           MOVE WRK-TI-HHMMSS          TO WRK-TI-HHMMSS-RDF.
           MOVE WRK-TI-HH              TO CB2-TI-HH.
           MOVE WRK-TI-MM              TO CB2-TI-MM
           MOVE WRK-TI-SS              TO CB2-TI-SS.
           MOVE CABEC2                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC3                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC4                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC5                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC6                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC7                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC8                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE 8                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2200-EMITIR-REG                 SECTION.
      *----------------------------------------------------------------*

           MOVE CABEC9                 TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-LINHAS.

           MOVE  BVVEH0H-TIPO-REG-X      TO LD1-TP-REG.
           MOVE  BVVEH0H-BANCO-ENVIO-X   TO LD1-BCO-ENV.
           MOVE  BVVEH0H-DATA-ENVIO-X    TO LD1-DT-ENV.
           MOVE  BVVEH0H-HORA-ENVIO-X    TO LD1-HORA-ENV.
           MOVE  BVVEH0H-DATA-CTRL-X     TO LD1-DT-CTRL.
           MOVE  BVVEH0H-VERSAO-ARQ-X    TO LD1-VERSAO-ARQ.

           MOVE  SPACES                TO LD1-PV
                                          LD1-CNPJ-CPF-ESTBL
                                          LD1-F-J
                                          LD1-CAGEN
                                          LD1-CCTA
                                          LD1-CCTA-DIG.
           IF  BVVEH0H-TIPO-REG-X        EQUAL '1'

               MOVE BVVEH0D-PONTO-VENDA-X
                                       TO LD1-PV
               MOVE BVVEH0D-CNPJ       TO LD1-CNPJ-CPF-ESTBL
               MOVE BVVEH0D-TIPO-PESSOA
                                       TO LD1-F-J
               MOVE BVVEH0D-AGENCIA-X    TO LD1-CAGEN
               MOVE BVVEH0D-CONTA-NRO-X  TO LD1-CCTA
               MOVE BVVEH0D-CONTA-DIG-X
                                       TO LD1-CCTA-DIG
           END-IF.

           MOVE LINDET1                TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-LINHAS.

           MOVE 1                      TO WRK-IND-ERROS.

           PERFORM  UNTIL WRK-IND-ERROS EQUAL 20
               IF  BVVEH0-TAB-ERRO(WRK-IND-ERROS)
                                       EQUAL 1
                   IF  ACU-LINHAS      GREATER 52
                       PERFORM 2100-EMITIR-CABEC
                   END-IF
                   MOVE BVVEH4-TB-ERRODESCR(WRK-IND-ERROS)
                                       TO LD2-DESC-ERRO
                   MOVE LINDET2        TO LINHA-RELATO
                   WRITE LINHA-RELATO
                   PERFORM 1120-TESTAR-FS-RELATO
                   MOVE SPACES         TO LINDET2
                   ADD 1               TO ACU-GRAVADOS
                                          ACU-LINHAS
               END-IF
               ADD 1                   TO WRK-IND-ERROS
           END-PERFORM.

           ADD 1                       TO ACU-PROPOSTAS.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-EMITIR-TOTAL               SECTION.
      *----------------------------------------------------------------*

           MOVE CABEC9                TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE CABEC8                TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

           MOVE ACU-PROPOSTAS          TO WRK-DESCOMPACTA.
           MOVE WRK-DESCOMPACTA        TO LT1-TTL-REG-INCONSIS.
           MOVE LINTOT1                TO LINHA-RELATO.
           WRITE LINHA-RELATO.
           PERFORM 1120-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZACAO                SECTION.
      *----------------------------------------------------------------*

           PERFORM 2300-EMITIR-TOTAL.

           CLOSE CDOMERRO
                 RELATO.

           MOVE    WRK-FECHAMENTO           TO  WRK-OPERACAO.
           PERFORM 1120-TESTAR-FS-RELATO.
           PERFORM 1110-TESTAR-FS-CDOMERRO.

           DISPLAY '***************** B V V E 6 4 5 0 ****************'.
           DISPLAY '*         RESUMO DO PROCESSAMENTO                *'.
           DISPLAY '* TOTAL DE REGS.LIDOS CONF. DOMIC./ ERRO :       *'.
           DISPLAY '*              ' ACU-LIDOS '          *'.
           DISPLAY '* TOTAL DE REGS.GRAVADOS NO RELATORIO......:     *'.
           DISPLAY '*              ' ACU-GRAVADOS '       *'.
           DISPLAY '***************** B V V E 6 4 5 0 ****************'.

           STOP RUN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-TRATAR-ERRO            SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE6450'           TO ERR-PGM
           MOVE 'APL'                TO ERR-TIPO-ACESSO
           MOVE SPACES               TO WRK-ERR-DADOS-SENHAS

           CALL 'BRAD7100' USING WRK-BATCH
                                 ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
