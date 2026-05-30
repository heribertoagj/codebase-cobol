      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2482.
       AUTHOR MARCUS VINICIUS.
      *================================================================*
      *                 W I P R O   I N F O S E R V E R                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2482                                     *
      *    ANALISTA     : MARCUS VINICIUS - INFOSERVER - GRUPO 38      *
      *    DATA         : FEV/2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *       ACRESCENTA INFORMACAO DO CPF/CNPJ DO CLIENTE             *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      EVENTOSE                              I#GFCTVE            *
      *      EVENTCPF                              --------            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - MODULO TRATAMENTO DE ERRO.                     *
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

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT EVENTOSE ASSIGN      TO UT-S-EVENTOSE
                      FILE STATUS      IS WRK-FS-EVENTOSE.

           SELECT EVENTCPF ASSIGN      TO UT-S-EVENTCPF
                      FILE STATUS      IS WRK-FS-EVENTCPF.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:  ADESOES DAS CESTAS NEXT  - 1184 E 1187               *
      *           ORG.SEQUENCIAL  -  LRECL = 094                       *
      *----------------------------------------------------------------*

       FD  EVENTOSE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
      *   OUTPUT: ADESOES DAS CESTAS NEXT - SOMENTE DO PROPRIO MES     *
      *           ORG.SEQUENCIAL  -  LRECL = 104                       *
      *----------------------------------------------------------------*

       FD  EVENTCPF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-EVENTCPF-REG             PIC X(300).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT2482 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-REG-LIDOS            PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-GRAVADOS         PIC 9(012) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(008)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

           05  WRK-CPF-CNPJ            PIC S9(009)V        VALUE ZEROS.

           05  WRK-FILIAL              PIC S9(005)V        VALUE ZEROS.

           05  WRK-CONTROLE            PIC S9(002)V        VALUE ZEROS.
           05  WRK-CONTROLE-AUX        REDEFINES WRK-CONTROLE
                                       PIC  9(002).

           05  WRK-CONTROLE-X.
               07  WRK-CONTROLE-N      PIC  9(002)         VALUE ZEROS.


           05  WRK-ERRO                PIC  9(001)         VALUE ZEROS.

           05  WRK-CID-CLI             PIC  X(026)         VALUE SPACES.
           05  WRK-CTPO-CTA            PIC  9(002)         VALUE ZEROS.

           05  WRK-CONTA-13            PIC S9(013)V        VALUE ZEROS.
           05  WRK-CONTA-AUX-13        REDEFINES WRK-CONTA-13.
               07  WRK-CONTA-AUX-6     PIC  9(006).
               07  WRK-CONTA-AUX-7     PIC  9(007).

           05  WRK-CONTA               PIC S9(007)V        VALUE ZEROS.
           05  WRK-CONTA-AUX           REDEFINES WRK-CONTA
                                       PIC  9(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  AREA FILE-STATUS            *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EVENTOSE             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EVENTCPF             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(007)          VALUE SPACES.
           05  FILLER                  PIC X(007)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(013)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(017)          VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CLIEV007
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CLIEV008
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CLIEV004
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2482   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0500-VERIFICA-VAZIO.

           PERFORM 1000-PROCESSA-MOVIMENTO-EXTF
                    UNTIL     WRK-FS-EVENTOSE   EQUAL '10'.

           PERFORM 1800-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  EVENTOSE
                OUTPUT EVENTCPF.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-EVENTOSE.

           PERFORM 0400-TESTAR-FS-EVENTCPF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO EVENTOSE                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-EVENTOSE         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EVENTOSE          NOT EQUAL '00'
              MOVE 'EVENTOSE'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-EVENTOSE     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO EVENTCPF                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-EVENTCPF         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EVENTCPF          NOT EQUAL '00'
              MOVE 'EVENTCPF'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-EVENTCPF     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS            *
      *----------------------------------------------------------------*
       0500-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 0600-LER-ARQUIVO-EVENTOSE.

           IF  WRK-FS-EVENTOSE         EQUAL '10'
               DISPLAY '***************** GFCT2482 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*      ARQUIVO COM EVENTOS COM CESTAS NEXT *'
               DISPLAY '*          VAZIO !!!                       *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2482 *****************'
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO EVENTOSE                                *
      *----------------------------------------------------------------*
       0600-LER-ARQUIVO-EVENTOSE       SECTION.
      *----------------------------------------------------------------*

           READ EVENTOSE.

           IF  WRK-FS-EVENTOSE         EQUAL '10'
               GO                      TO 0600-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-EVENTOSE.

           ADD 1                       TO ACU-REG-LIDOS.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       1000-PROCESSA-MOVIMENTO-EXTF    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1500-GRAVA-REGISTRO.

           PERFORM 0600-LER-ARQUIVO-EVENTOSE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO SAIDA                                       *
      *----------------------------------------------------------------*
       1500-GRAVA-REGISTRO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 1600-OBTER-DADOS-CLIENTE.
           MOVE WRK-CPF-CNPJ           TO  VE-CPF-CNPJ.
           MOVE WRK-FILIAL             TO  VE-FILIAL.
           MOVE WRK-CONTROLE-AUX       TO  WRK-CONTROLE-N.
           MOVE WRK-CONTROLE-X         TO  VE-CONTROLE.

           WRITE FD-EVENTCPF-REG       FROM  REG-IGFCTVE.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-EVENTCPF.

           ADD 1                       TO ACU-REG-GRAVADOS.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O CPJ / CNPJ                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-OBTER-DADOS-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  WRK-ERRO

           PERFORM 1620-ACESSAR-CTA-CORRENTE

           IF  WRK-ERRO                EQUAL ZEROS
               PERFORM 1630-ACESSAR-CLIEV004
           ELSE
               MOVE    ZEROS           TO  WRK-ERRO
               PERFORM 1610-ACESSAR-CTA-POUPANCA

               IF  WRK-ERRO            EQUAL ZEROS
                   PERFORM 1630-ACESSAR-CLIEV004
               END-IF
           END-IF.

           IF  WRK-ERRO                EQUAL  1
               MOVE ZEROS              TO WRK-CPF-CNPJ
               MOVE ZEROS              TO WRK-FILIAL
               MOVE ZEROS              TO WRK-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1610-ACESSAR-CTA-POUPANCA       SECTION.
      *----------------------------------------------------------------*

           MOVE VE-CAG-DSTNO-MOVTO     TO CJUNC-DEPDC OF CLIEV007.
           MOVE VE-CCTA-DSTNO-MOVTO    TO WRK-CONTA-13.
           MOVE WRK-CONTA-AUX-7        TO WRK-CONTA-AUX.
           MOVE WRK-CONTA              TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                    CCTA_CLI,
                    CID_CLI,
                    CTPO_CTA
             INTO
                   :CLIEV007.CCTA-CLI,
                   :CLIEV007.CID-CLI,
                   :CLIEV007.CTPO-CTA
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                    CJUNC_DEPDC         = :CLIEV007.CJUNC-DEPDC
             AND    CCTA_CLI            = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WRK-ERRO
           ELSE
               MOVE CID-CLI OF CLIEV007
                                       TO WRK-CID-CLI
               MOVE CTPO-CTA OF CLIEV007
                                       TO WRK-CTPO-CTA
           END-IF.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1620-ACESSAR-CTA-CORRENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE VE-CAG-DSTNO-MOVTO     TO CJUNC-DEPDC OF CLIEV008.
           MOVE VE-CCTA-DSTNO-MOVTO    TO WRK-CONTA-13.
           MOVE WRK-CONTA-AUX-7        TO WRK-CONTA-AUX.
           MOVE WRK-CONTA              TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT
                    CCTA_CLI,
                    CID_CLI,
                    CTPO_CTA
             INTO
                   :CLIEV008.CCTA-CLI,
                   :CLIEV008.CID-CLI,
                   :CLIEV008.CTPO-CTA
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                    CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC
             AND    CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WRK-ERRO
           ELSE
               MOVE CID-CLI OF CLIEV008
                                       TO WRK-CID-CLI
               MOVE CTPO-CTA OF CLIEV008
                                       TO WRK-CTPO-CTA
           END-IF.

      *----------------------------------------------------------------*
       1620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1630-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

           EXEC SQL
             SELECT
                    IPSSOA_COPLT,
                    CTPO_PSSOA,
                    CCGC_CPF,
                    CFLIAL_CGC,
                    CCTRL_CPF_CGC,
                    CCGC_CPF_ST,
                    CFLIAL_CGC_ST,
                    CCTRL_CPF_CGC_ST                    
             INTO
                   :CLIEV004.IPSSOA-COPLT,
                   :CLIEV004.CTPO-PSSOA,
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
                   :CLIEV004.CCGC-CPF-ST,
                   :CLIEV004.CFLIAL-CGC-ST,
                   :CLIEV004.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                    CID_CLI = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WRK-ERRO
           ELSE
               MOVE CCGC-CPF           OF CLIEV004
                                       TO WRK-CPF-CNPJ
               MOVE CFLIAL-CGC         OF CLIEV004
                                       TO WRK-FILIAL
               MOVE CCTRL-CPF-CGC      OF CLIEV004
                                       TO WRK-CONTROLE

ST25X7         MOVE CCGC-CPF-ST        OF CLIEV004
ST25X7                                 TO VE-CPF-CNPJ-ST 
ST25X7         MOVE CFLIAL-CGC-ST      OF CLIEV004
ST25X7                                 TO VE-FILIAL-ST 
ST25X7         MOVE CCTRL-CPF-CGC-ST   OF CLIEV004
ST25X7                                 TO VE-CONTROLE-ST 
           END-IF.

      *----------------------------------------------------------------*
       1630-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       1800-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1900-EMITIR-DISPLAY-TOTAIS.

           CLOSE EVENTOSE
                 EVENTCPF.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR TOTAIS DE PROCESSAMENTO
      *----------------------------------------------------------------*
       1900-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '                                               '.
           DISPLAY '***************** GFCT2482 ********************'.
           DISPLAY '*                                             *'.
           DISPLAY '*            PROGRAMA GFCT2482                *'.
           DISPLAY '*                                             *'.
           DISPLAY '*---------------------------------------------*'.

           IF  ACU-REG-LIDOS           EQUAL ZEROS
               DISPLAY '*                                             *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO             *'
           ELSE
               DISPLAY '*                                             *'
               DISPLAY '*   PROCESSAMENTO ENCERRADO COM SUCESSO       *'
               DISPLAY '*                                             *'
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*                                             *'

               MOVE ACU-REG-LIDOS          TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS EVENTOSE.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-GRAVADOS       TO WRK-MASCARA

               DISPLAY '*  REGISTROS GRAV. EVENTCPF.: ' WRK-MASCARA
                                                               ' *'

           END-IF.

           DISPLAY '*                                             *'.
           DISPLAY '***************** GFCT2482 ********************'.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2482'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
