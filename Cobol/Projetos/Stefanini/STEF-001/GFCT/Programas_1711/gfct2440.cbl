      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2440.
       AUTHOR.     LAYS MOLEIRO.
      *================================================================*
      *                    S O N D A  P R O C W O R K                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2440                                    *
      *    PROGRAMADOR.:   LAYS MOLEIRO      -  SONDA PROCWORK - GP.50 *
      *    ANALISTA....:   MARCUS VINICIUS   -  SONDA PROCWORK - GP.50 *
      *    DATA........:   04/12/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   IDENTIFICAR QUEM E CORRESPONDENTE BANCARIO. *
      *                    TRAZENDO STO.                               *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                        INCLUDE/BOOK      *
      *                  ADESCOMP                      I#GFCTSX        *
      *                  ADECOMCB                      GFCTWAJ1        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *               DB2                                              *
      *                TABLE                         INCLUDE/BOOK      *
      *                DB2PRD.TCAD_CORSP_BCRIO       YMKCB001          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTSX - ENTRADA - ADESAO COMPLETA   CESTA DE SERVICOS    *
      *    GFCTWAJ1 - SAIDA   - ADESAO COMPLETA   CORRESP BANCARIO     *
      *    I#BRAD7C - AREA UTILIZADA PELA BRAD7100.                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - FAZ CONEXAO COM DB2.                             *
      *    BRAD7100 - MODULO PARA TRATAMENTO DE ERROS.                 *
      *    MESU9018 - CONSISTIR PAB.                                   *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  06 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
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

           SELECT ADESCOMP ASSIGN      TO UT-S-ADESCOMP
           FILE STATUS                 IS WRK-FS-ADESCOMP.

           SELECT ADECOMCB ASSIGN      TO UT-S-ADECOMCB
           FILE STATUS                 IS WRK-FS-ADECOMCB.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : ARQUIVO ENTRADA                                     *
      *            ORG. SEQUENCIAL     -   LRECL   =   030             *
      *----------------------------------------------------------------*

       FD  ADESCOMP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTSX'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO SAIDA                                       *
      *            ORG. SEQUENCIAL     -   LRECL   =   060             *
      *----------------------------------------------------------------*

       FD  ADECOMCB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWAJ1'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ADESCOMP          PIC  9(009)  COMP-3 VALUE ZEROS.
       77  ACU-GRAVADOS-ADECOMCB       PIC  9(009)  COMP-3 VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(009)  COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-FIM                     PIC  X(001)         VALUE SPACES.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-CORRESP                 PIC  X(001)         VALUE 'N'  .
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01 WRK-S-910                    PIC +9(10)          VALUE ZEROS.
       01 WRK-FILLER                   REDEFINES WRK-S-910.
          05 FILLER                    PIC X(01).
          05 WRK-910                   PIC 9(10).

       01  WRK-CPAB-9-05               PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-CPAB-9-05.
           05 FILLER                   PIC  9(02).
           05 WRK-CPAB-9-03            PIC  9(03).

       01  WRK-CLJ-BP15.
           05  FILLER                  PIC  X(007)         VALUE ZEROS.
           05  WRK-CLJ-BP8             PIC  X(008)         VALUE SPACES.

       01  WRK-CLJ-BE15.
           05  FILLER                  PIC  X(010)         VALUE ZEROS.
           05  WRK-CLJ-BE8             PIC  X(005)         VALUE SPACES.

VI0910 01  WRK-STO-BP.
VI0910     05  FILLER                  PIC  9(007)         VALUE ZEROS.
VI0910     05  WRK-CLJ-CORSP-BP        PIC  X(008)         VALUE SPACES.

VI0910 01  WRK-STO-BE.
VI0910     05  FILLER                  PIC  9(010)         VALUE ZEROS.
VI0910     05  WRK-CLJ-CORSP-BE        PIC  X(005)         VALUE SPACES.

       01  WRK-CHAVE-ATU.
           05  WRK-AGENCIA-ATU         PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-PAB-ATU             PIC  9(005) COMP-3  VALUE ZEROS.

       01  WRK-CHAVE-ANT.
           05  WRK-AGENCIA-ANT         PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-PAB-ANT             PIC  9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ADESCOMP             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ADECOMCB             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           'NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA FORMATACAO DE ERROS *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05  FILLER                  PIC  X(07)          VALUE SPACES.
           05  FILLER                  PIC  X(07)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           05  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(02)          VALUE ' *'.
           05  FILLER                  PIC  X(07)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM KCIM0184           *'.
      *----------------------------------------------------------------*

       COPY KCIMW184.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA BRAD7100 *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MESU9018 *'.
      *----------------------------------------------------------------*

        COPY I#MESUM8.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    INICIA OS PROCEDIMENTOS A SEREM EXECUTADOS                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FIM             EQUAL 'S'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ADESCOMP
                OUTPUT ADECOMCB.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-ADESCOMP.

           PERFORM 1220-TESTAR-FS-ADECOMCB.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DO ARQUIVO ADESCOMP                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TESTAR-FS-ADESCOMP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADESCOMP         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADESCOMP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADESCOMP    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DO ARQUIVO ADECOMCB                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TESTAR-FS-ADECOMCB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADECOMCB         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ADECOMCB'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ADECOMCB    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-BRAD7100  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR SE O ARQUIVO ESTA VAZIO                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ADESCOMP.

           IF  WRK-FS-ADESCOMP         EQUAL '10'
               DISPLAY '********* GFCT2440 *********'
               DISPLAY '*                          *'
               DISPLAY '*  ARQUIVO ADESCOMP VAZIO  *'
               DISPLAY '*  PROCESSAMENTO ENCERRADO *'
               DISPLAY '*                          *'
               DISPLAY '********* GFCT2440 *********'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER O ARQUIVO ADESCOMP.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ADESCOMP               SECTION.
      *----------------------------------------------------------------*

           READ ADESCOMP.

           IF  WRK-FS-ADESCOMP         EQUAL '10'
               MOVE 'S'                TO WRK-FIM
               GO TO                   2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-ADESCOMP.

           MOVE GFCTSX-AGENCIA               TO WRK-AGENCIA-ATU
           MOVE GFCTSX-PAB                   TO WRK-PAB-ATU

           ADD 1                        TO ACU-LIDOS-ADESCOMP.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-ATU           NOT EQUAL WRK-CHAVE-ANT
               PERFORM 3050-ACESSO-MESU9018
               MOVE WRK-CHAVE-ATU      TO WRK-CHAVE-ANT
           END-IF.

           IF  WRK-CORRESP             EQUAL  'S'
               PERFORM 3250-TRATAR-SAIDA
               PERFORM 3300-GRAVAR-ADECOMCB
           ELSE
               ADD  1                  TO ACU-DESPREZADOS
           END-IF.

           PERFORM 2100-LER-ADESCOMP.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3050-ACESSO-MESU9018            SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'                 TO COMU-VERSAO
           MOVE 237                      TO COMU-COD-EMPR
           MOVE GFCTSX-AGENCIA                TO COMU-COD-DEPDC
           MOVE GFCTSX-PAB                    TO COMU-COD-POSTO
           MOVE SPACES                   TO COMU-CORR-BANCARIO

           MOVE 'MESU9018'             TO WRK-MODULO

           CALL WRK-MODULO              USING COMU-DADOS-9018

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'N'                TO WRK-CORRESP
           ELSE
              IF  COMU-COD-TIPO-POSTO     NOT EQUAL 9
                  MOVE 'N'                TO WRK-CORRESP
              ELSE
                  INITIALIZE KCIMW184-AREA-COMUNICACAO

                  MOVE 01                TO KCIMW184-E-FUNCAO
                  MOVE GFCTSX-AGENCIA    TO KCIMW184-E-CAG-BCRIA-1
                  MOVE GFCTSX-PAB        TO WRK-CPAB-9-05
                  MOVE WRK-CPAB-9-03     TO KCIMW184-E-CPOSTO-BCRIO-1

                  PERFORM 3100-ACESSAR-YMKCB001
                  PERFORM 3200-TRATAR-CORRESP
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
       3100-ACESSAR-YMKCB001        SECTION.
      *----------------------------------------------------------------*

           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO            USING KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE '0010'              TO ERR-LOCAL
               MOVE SPACES              TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
       3200-TRATAR-CORRESP            SECTION.
      *----------------------------------------------------------------*

           IF  KCIMW184-S-COD-RETORNO EQUAL 98
               MOVE 'N'                TO WRK-CORRESP
           ELSE
               MOVE 'S'                TO WRK-CORRESP
           END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3250-TRATAR-SAIDA               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTSX-AGENCIA           TO WAJ1-AGENCIA.
           MOVE GFCTSX-CONTA             TO WAJ1-CONTA.
           MOVE GFCTSX-PAB               TO WAJ1-PAB.
           MOVE GFCTSX-PACOTE            TO WAJ1-PACOTE.

           MOVE KCIMW184-S-CEMPR-CORSP (1) TO WRK-S-910.
           MOVE WRK-910                    TO WAJ1-EMPRESA.

VI0910     IF  KCIMW184-S-CEMPR-CORSP (1)  EQUAL  10000
VI0910         MOVE KCIMW184-S-CLJ-CORSP (1)
VI0910                                   TO WRK-CLJ-BP15
               MOVE WRK-CLJ-BP8          TO WRK-CLJ-CORSP-BP
VI0910         MOVE WRK-STO-BP           TO WAJ1-STO
VI0910     ELSE
VI0910         MOVE KCIMW184-S-CLJ-CORSP (1)
VI0910                                   TO WRK-CLJ-BE15
               MOVE WRK-CLJ-BE8          TO WRK-CLJ-CORSP-BE
VI0910         MOVE WRK-STO-BE           TO WAJ1-STO
VI0910     END-IF.

      *----------------------------------------------------------------*
       3250-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * GRAVAR REGISTRO NO ARQUIVO DE SAIDA ADECOMCB                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-GRAVAR-ADECOMCB            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO            TO WRK-OPERACAO.

           WRITE WAJ1-ENTRADA.

           PERFORM 1220-TESTAR-FS-ADECOMCB.

           ADD 1                        TO ACU-GRAVADOS-ADECOMCB.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    PROCEDIMENTOS FINAIS DO PROGRAMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ADESCOMP
                 ADECOMCB.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXIBE MENSAGEM DE FINAL DE PROCESSAMENTO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                PROGRAMA GFCT2440               *'.
           DISPLAY '*                -----------------               *'.
           DISPLAY '*                                                *'.
           DISPLAY '*       PROCESSAMENTO EFETUADO COM SUCESSO       *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ADESCOMP     TO WRK-MASCARA.
           DISPLAY '* REGS LIDOS ADESCOMP      = ' WRK-MASCARA
                                                           '         *'.
           MOVE ACU-GRAVADOS-ADECOMCB  TO WRK-MASCARA.
           DISPLAY '* REGS GRAVADOS ADECOMCB   = ' WRK-MASCARA
                                                           '         *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.
           DISPLAY '* REGS DESPREZADOS         = ' WRK-MASCARA
                                                           '         *'.
           DISPLAY '*                                                *'.
           DISPLAY '*------------------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA TRATAMENTO DE ERROS                             *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2440'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
