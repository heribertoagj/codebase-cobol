      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1643.
       AUTHOR.     RODRIGO GUSTAVO DA SILVA.
      *================================================================*
      *                     C P M   S I S T E M A S                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1643                                     *
      *    PROGRAMADOR  : RODRIGO GUSTAVO DA SILVA       - CPM/FPOLIS  *
      *    ANALISTA CPM : MARCIO RODRIGO DA CUNHA        - CPM/FPOLIS  *
      *    ANALISTA     : MARIA VALERIA - PROCWORK       - GRUPO 50    *
      *    DATA         : 02/02/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      COMPLETAR OS REGISTROS POR EVENTO, COM OS ARGUMENTOS.     *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                               INCLUDE/BOOK         *
      *      EVTOAVER                               --------           *
      *      EVENTOAT                               --------           *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                INCLUDE/BOOK         *
      *      DB2PRD.V01CLIENTE_AGENCIA              CLIEV004           *
      *      DB2PRD.V01CTA_POUPANCA                 CLIEV007           *
      *      DB2PRD.V01CTA_CORRENTE                 CLIEV008           *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL1050 - CONEXAO COM DB2.                               *
      *      POOL7100 - MODULO TRATAMENTO DE ERRO.                     *
      *      GFCT5564 - MODULO PARA OBTER CODIGO DO MUNICIPIO E SIGLA  *
      *                 DA UF DA AGENCIA.                              *
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

           SELECT EVTOAVER ASSIGN      TO UT-S-EVTOAVER
                      FILE STATUS      IS WRK-FS-EVTOAVER.

           SELECT EVENTOAT ASSIGN      TO UT-S-EVENTOAT
                      FILE STATUS      IS WRK-FS-EVENTOAT.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT : REGISTRO EVENTO DO DIA TABELA GFCTB092               *
      *           ORG.SEQUENCIAL       LRECL - 073                     *
      *----------------------------------------------------------------*

       FD  EVTOAVER
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EVTOAVER             PIC X(73).

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTRO EVENTOS ATUALIZADOS GFCTB092                *
      *           ORG.SEQUENCIAL       LRECL - 093                     *
      *----------------------------------------------------------------*

       FD  EVENTOAT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EVENTOAT             PIC X(93).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT1643 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-EVTOAVER       PIC 9(011) COMP-3   VALUE ZEROS.
           05 ACU-GRAV-EVENTOAT        PIC 9(011) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(08)           VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZZ.ZZZ.999    VALUE ZEROS.
           05  WRK-MODULO              PIC X(08)           VALUE SPACES.
           05  WRK-CID-CLI             PIC X(26)           VALUE SPACES.
           05  WRK-COD-POSTO           PIC S9(03)V COMP-3  VALUE ZEROS.
           05  WRK-CTA-AUX-13          PIC 9(13)           VALUE ZEROS.
           05  FILLER                  REDEFINES         WRK-CTA-AUX-13.
               10  FILLER              PIC 9(06).
               10  WRK-CTA-AUX-7       PIC 9(07).
           05  WRK-MASC-11             PIC 9(11)           VALUE ZEROS.
           05  FILLER                  REDEFINES           WRK-MASC-11.
               10  FILLER              PIC 9(01).
               10  WRK-MASC-10         PIC 9(10).

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* AREA PARA CHAVES             *'.
      *---------------------------------------------------------------*

       01  WRK-CH-MOVEVNTO.
           05  WRK-MOV-CAG-DSTNO-MOVTO PIC S9(05) COMP-3   VALUE ZEROS.
           05  WRK-MOV-CCTA-DSTNO-MOVTO
                                       PIC S9(13) COMP-3   VALUE ZEROS.

       01  WRK-CH-ANT.
           05  WRK-MOV-CAG-ANT         PIC S9(05) COMP-3   VALUE ZEROS.
           05  WRK-MOV-CCTA-ANT        PIC S9(13) COMP-3   VALUE ZEROS.

       01  WRK-CCTRL-CPF               PIC 9(02)           VALUE ZEROS.
       01  WRK-CCTRL-CPF-R             REDEFINES         WRK-CCTRL-CPF.
           05  WRK-CCTRL-CPF-AUX       PIC X(02).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-EVTOAVER             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EVENTOAT             PIC  X(002)         VALUE SPACES.
       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           'NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  MENSAGENS DE ERRO           *'.
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
           05  WRK-FS                  PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DO ARQUIVO EVTOAVER    *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYJ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DO ARQUIVO EVENTOAT   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTYL.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DE ERROS         *'.
      *----------------------------------------------------------------*

       COPY I#GFCT0M.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DO MODULO GFCT5564    *'.
      *----------------------------------------------------------------*

       COPY I#GFCTRK.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA PARA POOL7100    *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
               INCLUDE CLIEV008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT1643  *'.
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

           PERFORM 0500-VERIFICAR-VAZIO.

           PERFORM 0700-PROCESSAR-DADOS UNTIL
                   WRK-FS-EVTOAVER     EQUAL '10'.

           PERFORM 1500-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT   EVTOAVER
                OUTPUT  EVENTOAT.

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

           PERFORM 0300-TESTAR-FS-EVTOAVER.

           PERFORM 0400-TESTAR-FS-EVENTOAT.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO EVTOAVER                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-EVTOAVER         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EVTOAVER          NOT EQUAL '00'
              MOVE 'EVTOAVER'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-EVTOAVER     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO EVENTOAT                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-EVENTOAT         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EVENTOAT          NOT EQUAL '00'
              MOVE 'EVENTOAT'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-EVENTOAT     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      ROTINA PARA VERIFICAR ARQUIVO VAZIO                       *
      *----------------------------------------------------------------*
       0500-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0600-LER-EVTOAVER.

           IF  WRK-FS-EVTOAVER         EQUAL '10'
               DISPLAY '*----------------------------------------------'
                     '-*'
               DISPLAY '*            PROGRAMA GFCT1643                 '
                     ' *'
               DISPLAY '*            -----------------                 '
                     ' *'
               DISPLAY '*                                              '
                     ' *'
               DISPLAY '*         ARQUIVO DE EVTOAVER VAZIO            '
                     ' *'
               DISPLAY '*----------------------------------------------'
                     '-*'
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   LEITURA DO ARQUIVO EVTOAVER                                  *
      *----------------------------------------------------------------*
       0600-LER-EVTOAVER               SECTION.
      *----------------------------------------------------------------*

           READ EVTOAVER               INTO GFCTYJ-GFCTB092.

           IF WRK-FS-EVTOAVER          EQUAL '10'
               GO                      TO 0600-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-EVTOAVER.

           ADD 1                       TO ACU-LIDOS-EVTOAVER.

           MOVE GFCTYJ-CAG-DSTNO-MOVTO TO WRK-MOV-CAG-DSTNO-MOVTO.
           MOVE GFCTYJ-CCTA-DSTNO-MOVTO
                                       TO WRK-MOV-CCTA-DSTNO-MOVTO.
           MOVE GFCTYJ-GFCTB092        TO GFCTYL-GFCTB092.

           INITIALIZE                  GFCTYL-AGRUPAMENTOS.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       0700-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CH-ANT              NOT EQUAL WRK-CH-MOVEVNTO
               PERFORM 0800-PESQUISAR-AGRUPAMENTOS
           END-IF.

           MOVE GFCTRK-CMUN-RURAL      TO GFCTYL-CMUN-RURAL.
           MOVE GFCTRK-CSGL-UF         TO GFCTYL-CSGL-UF.
           MOVE WRK-COD-POSTO          TO GFCTYL-CPOSTAL.
ST25X6*    MOVE CCGC-CPF OF CLIEV004   TO GFCTYL-CPF-CNPJ.
ST25X6     MOVE CCGC-CPF-ST OF CLIEV004 TO GFCTYL-CPF-CNPJ.
ST25X6*    MOVE CFLIAL-CGC OF CLIEV004 TO GFCTYL-FILIAL.
ST25X6     MOVE CFLIAL-CGC-ST OF CLIEV004 TO GFCTYL-FILIAL.
           MOVE CCTRL-CPF-CGC OF CLIEV004
                                       TO WRK-CCTRL-CPF.
           MOVE WRK-CCTRL-CPF-AUX      TO GFCTYL-CONTROLE.

           PERFORM 1400-GRAVAR-EVENTOS.

           PERFORM 0600-LER-EVTOAVER.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PESQUISAR AGRUPAMENTOS.                         *
      *----------------------------------------------------------------*
       0800-PESQUISAR-AGRUPAMENTOS     SECTION.
      *----------------------------------------------------------------*

           PERFORM 0900-CHAMA-GFCT5564.

           PERFORM 1000-PESQUISA-CLIE.

           MOVE WRK-CH-MOVEVNTO        TO WRK-CH-ANT.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR O MODULO GFCT5564                        *
      *----------------------------------------------------------------*
       0900-CHAMA-GFCT5564             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCTRK-AREA-RETORNO
                                       GFCT0M-AREA-ERROS.

           MOVE 'B'                    TO GFCTRK-TPO-AMBIENTE.
           MOVE GFCTYJ-CAG-DSTNO-MOVTO TO GFCTRK-CJUNC-DEPDC.
           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*   PROGRAMA GFCT1643                *'
               DISPLAY '*   -----------------                *'
               DISPLAY '*   ERRO NO ACESSO AO MODULO         *'
               DISPLAY '*   GFCT5564 PELA AGENCIA:           *'
               DISPLAY '*   ' GFCTYJ-CAG-DSTNO-MOVTO '                '
                       '           *'
               DISPLAY '************** GFCT1643 **************'
               PERFORM 1500-PROCEDIMENTOS-FINAIS
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 01
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*  PROGRAMA GFCT1643                 *'
               DISPLAY '*  -----------------                 *'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564 *'
               DISPLAY '*  TIPO AMBIENTE INVALIDO            *'
               DISPLAY '************** GFCT1643 **************'
               PERFORM 1500-PROCEDIMENTOS-FINAIS
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 02
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*  PROGRAMA GFCT1643                 *'
               DISPLAY '*  -----------------                 *'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564 *'
               DISPLAY '*  COD AGENCIA NAO NUMERICO OU ZEROS *'
               DISPLAY '************** GFCT1643 **************'
               PERFORM 1500-PROCEDIMENTOS-FINAIS
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 99
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*  PROGRAMA GFCT1643                 *'
               DISPLAY '*  -----------------                 *'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564 *'
               DISPLAY '*  ERRO DB2 TABELA:                  *'
               DISPLAY '*  ' GFCT0M-NOME-TAB '                *'
               DISPLAY '************** GFCT1643 **************'
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
*********      PERFORM 1500-PROCEDIMENTOS-FINAIS
           END-IF.

           IF  GFCTRK-COD-RETORNO      EQUAL 03
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*  PROGRAMA GFCT1643                 *'
               DISPLAY '*  -----------------                 *'
               DISPLAY '*  ERRO NO ACESSO AO MODULO GFCT5564 *'
               DISPLAY '*  AGENCIA INEXISTENTE:              *'
               DISPLAY '*  ' GFCTYJ-CAG-DSTNO-MOVTO '        '
                       '                    *'
               DISPLAY '*  MUNICIPIO E UF SERAO ZERADOS      *'
               DISPLAY '************** GFCT1643 **************'
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA PESQUISAR CLIENTES                             *
      *----------------------------------------------------------------*
       1000-PESQUISA-CLIE              SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-PESQUISA-CLIEV007.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1200-PESQUISA-CLIEV008
           END-IF.

           PERFORM 1300-PESQUISA-CLIEV004.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA ACESSAR TABELA CLIEV007                        *
      *----------------------------------------------------------------*
       1100-PESQUISA-CLIEV007          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYJ-CAG-DSTNO-MOVTO TO CJUNC-DEPDC OF CLIEV007.
           MOVE GFCTYJ-CCTA-DSTNO-MOVTO
                                       TO WRK-CTA-AUX-13.
           MOVE WRK-CTA-AUX-7          TO CCTA-CLI OF CLIEV007.

           EXEC SQL
               SELECT CID_CLI,
                      CPOSTO_SERVC
               INTO
                   :CLIEV007.CID-CLI,
                   :CLIEV007.CPOSTO-SERVC
               FROM DB2PRD.V01CTA_POUPANCA
               WHERE CJUNC_DEPDC       = :CLIEV007.CJUNC-DEPDC
                 AND CCTA_CLI          = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA   '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0001'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE CID-CLI OF CLIEV007
                                       TO WRK-CID-CLI
               MOVE CPOSTO-SERVC OF CLIEV007
                                       TO WRK-COD-POSTO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA ACESSAR TABELA CLIEV008                        *
      *----------------------------------------------------------------*
       1200-PESQUISA-CLIEV008          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYJ-CAG-DSTNO-MOVTO TO CJUNC-DEPDC OF CLIEV008.
           MOVE GFCTYJ-CCTA-DSTNO-MOVTO
                                       TO WRK-CTA-AUX-13.
           MOVE WRK-CTA-AUX-7          TO CCTA-CLI OF CLIEV008.

           EXEC SQL
               SELECT CID_CLI,
                      CPOSTO_SERVC
               INTO
                   :CLIEV008.CID-CLI,
                   :CLIEV008.CPOSTO-SERVC
               FROM DB2PRD.V01CTA_CORRENTE
               WHERE CJUNC_DEPDC       = :CLIEV008.CJUNC-DEPDC
                 AND CCTA_CLI          = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE   '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0002'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*   PROGRAMA GFCT1643                *'
               DISPLAY '*   -----------------                *'
               DISPLAY '*   ERRO NO ACESSO AO CLIE           *'
               DISPLAY '*   CLIENTE NAO ENCONTRADO           *'
               DISPLAY '*   PELA AGENCIA/CONTA :             *'
               DISPLAY '*   ' GFCTYJ-CAG-DSTNO-MOVTO '          '
                       '                 *'
               DISPLAY '*   ' GFCTYJ-CCTA-DSTNO-MOVTO '         '
                       '          *'
               DISPLAY '************** GFCT1643 **************'
               MOVE  ZEROS             TO WRK-CID-CLI
                                          WRK-COD-POSTO
***********    PERFORM 1500-PROCEDIMENTOS-FINAIS
           ELSE
               MOVE CID-CLI OF CLIEV008
                                       TO WRK-CID-CLI
               MOVE CPOSTO-SERVC OF CLIEV008
                                       TO WRK-COD-POSTO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PARA ACESSAR TABELA CLIEV004                        *
      *----------------------------------------------------------------*
       1300-PESQUISA-CLIEV004          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CID-CLI            TO CID-CLI OF CLIEV004.

           EXEC SQL
             SELECT CCGC_CPF,
                    CFLIAL_CGC,
                    CCTRL_CPF_CGC,
                    CTPO_PSSOA,
                    IPSSOA_COPLT,
ST25X6              CCGC_CPF_ST,
ST25X6              CFLIAL_CGC_ST,
ST25X6              CCTRL_CPF_CGC_ST
             INTO   :CLIEV004.CCGC-CPF,
                    :CLIEV004.CFLIAL-CGC,
                    :CLIEV004.CCTRL-CPF-CGC,
                    :CLIEV004.CTPO-PSSOA,
                    :CLIEV004.IPSSOA-COPLT,
ST25X6              :CLIEV004.CCGC-CPF-ST,
ST25X6              :CLIEV004.CFLIAL-CGC-ST,
ST25X6              :CLIEV004.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE  CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'CLIENTE_AGENCIA   '
                                       TO ERR-DBD-TAB
                MOVE 'SELECT    '      TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0003'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               DISPLAY '************** GFCT1643 **************'
               DISPLAY '*   PROGRAMA GFCT1643                *'
               DISPLAY '*   -----------------                *'
               DISPLAY '*   ERRO NO ACESSO AO CLIE           *'
               DISPLAY '*   CLIENTE NAO ENCONTRADO           *'
               DISPLAY '*   PELA AGENCIA/CONTA :             *'
               DISPLAY '*   ' GFCTYJ-CAG-DSTNO-MOVTO '          '
                       '                 *'
               DISPLAY '*   ' GFCTYJ-CCTA-DSTNO-MOVTO '         '
                       '          *'
               DISPLAY '************** GFCT1643 **************'
************   PERFORM 1500-PROCEDIMENTOS-FINAIS
               MOVE  ZEROS  TO CFLIAL-CGC    OF CLIEV004
               MOVE  ZEROS  TO CCGC-CPF      OF CLIEV004
               MOVE  ZEROS  TO CCTRL-CPF-CGC OF CLIEV004
ST25X6         MOVE  SPACES TO CFLIAL-CGC-ST OF CLIEV004
ST25X6         MOVE  SPACES TO CCGC-CPF-ST   OF CLIEV004
ST25X6         MOVE  SPACES TO CCTRL-CPF-CGC-ST OF CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVACAO DO ARQUIVO CTACANCD                               *
      *----------------------------------------------------------------*
       1400-GRAVAR-EVENTOS             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-EVENTOAT       FROM GFCTYL-GFCTB092.

           PERFORM 0400-TESTAR-FS-EVENTOAT.

           ADD 1                       TO ACU-GRAV-EVENTOAT.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       1500-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-EVTOAVER      GREATER ZEROS
               PERFORM 1600-EMITIR-DISPLAY
           END-IF.

           CLOSE EVTOAVER
                 EVENTOAT.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR DISPLAYS TOTAIS                                     *
      *----------------------------------------------------------------*
       1600-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-EVTOAVER     TO WRK-MASC-11.
           MOVE WRK-MASC-10            TO WRK-MASCARA.

           DISPLAY '*-----------------------------------------------*'
           DISPLAY '*            PROGRAMA GFCT1643                  *'
           DISPLAY '*            -----------------                  *'
           DISPLAY '*                                               *'
           DISPLAY '*      PROCESSAMENTO EFETUADO COM SUCESSO       *'
           DISPLAY '*-----------------------------------------------*'
           DISPLAY '**************************************************'
                   '***********************'
           DISPLAY '*        TOTAL DE REGISTROS LIDOS       NO EVENTOS'
                   '       ' WRK-MASCARA '   *'.

           MOVE ACU-GRAV-EVENTOAT      TO WRK-MASC-11.
           MOVE WRK-MASC-10            TO WRK-MASCARA.

           DISPLAY '*        TOTAL DE REGISTROS GRAVADOS    EM EVENTOS'
                   '-ATU   ' WRK-MASCARA '   *'
           DISPLAY '**************************************************'
                   '***********************'.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1643'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
