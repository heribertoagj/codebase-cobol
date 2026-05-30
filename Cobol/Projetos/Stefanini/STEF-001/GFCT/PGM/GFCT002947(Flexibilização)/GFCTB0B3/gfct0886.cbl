      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0886.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0886                                    *
      *    PROGRAMADOR.:   ANDRE LUIZ LONGO        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON LUIZ DO SANTOS    - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.75   *
      *    DATA........:   09/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - CANCELAMENTO FLEXIBILIZACAO*
      *      PARA AGRUPAMENTO NA TABELA DE PEDIDOS.                    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.PDIDO_FLEXZ_AG         GFCTB0B1       *
      *                   DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2       *
      *                   DB2PRD.PDIDO_FLEXZ_CLI        GFCTB0B3       *
      *                   DB2PRD.PDIDO_FLEXZ_MUN        GFCTB0B7       *
      *                   DB2PRD.PDIDO_FLEXZ_PAB        GFCTB0B8       *
      *                   DB2PRD.PDIDO_FLEXZ_PSTAL      GFCTB0B9       *
      *                   DB2PRD.PDIDO_FLEXZ_SGMTO      GFCTB0C0       *
      *                   DB2PRD.PDIDO_FLEXZ_UF         GFCTB0C1       *
110510*                   DB2PRD.TPDIDO_FLEXZ_RZ        GFCTB0M7       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
180808*                   DB2PRD.TPO_AGPTO_CTA          GFCTB0F3       *
      *                   DB2PRD.V01UF                  DCITV002       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTOV - ENTRADA                                          *
      *    I#GFCTOX - SAIDA                                            *
      *    I#GFCT1V - ENTRADA - ENTRADA DO MODULO GFCT5006.            *
      *    I#GFCT1X - SAIDA   - SAIDA DO MODULO GFCT5006.              *
      *    I#GFCTGB - ENTRADA - ENTRADA DO MODULO GFCT5109 E GFCT5090  *
      *    I#GFCTGC - SAIDA   - SAIDA DO MODULO GFCT5109 E GFCT5090    *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#RURC88 - AREA PARA O MODULO RURC9020.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5109 - OBTER DESCRICAO PAB.                             *
      *    GFCT5090 - OBTER DESCRICAO POSTO SERVICO.                   *
      *    GFCT5006 - OBTER DESCRICAO DEPENDENCIA.                     *
      *    DCIT8000 - VERIFICA UF                                      *
      *    RURC9020 - VERIFICA MUNICIPIO                               *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    POOL0110 - CONSISTIR CLIENTE (CPF)                          *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 01/04/2010 - HELIO SANTONI              *
0410  *    ALTERACOES NA INCLUSAO FLEXIBILIZACOES P/ AGRUP.            *
0410  *    - ACRESCENTAR TRATAMENTO DE CONJUNTO DE SERVICO AGRUPADO.   *
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
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.
       01  WRK-CONTROLE                PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-AUX             PIC  9(004).
           05  WRK-MES-AUX             PIC  9(002).
           05  WRK-DIA-AUX             PIC  9(002).

       01  WRK-DATA-INV2               PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV2-R             REDEFINES WRK-DATA-INV2.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREAS DE COMUNICACAO *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY 'I#RURC88'.

ST2506*------------------------------------*----------------------------*
ST2506 01 FILLER                       PIC  X(051) VALUE
ST2506       '* AREA PARA MODULO GFCT20PJ *'.
ST2506*---------------------------------------------------------------*
ST2506 01 WRK-GFCT20PJ                 PIC X(08)   VALUE
ST2506       'GFCT20PJ'.
ST2506
ST2506 01 WRK-AREA-GFCT20PJ.
ST2506 COPY 'GFCTW2PJ'.
ST2506

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000  *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002    PIC  X(070)     VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO     PIC  9(002)     VALUE ZEROS.

       01  WRK-ERRO-AREA                   PIC  X(107)     VALUE SPACES.
       01  WRK-SQLCA-DCIT                  PIC  X(136)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA TO TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

180808     EXEC SQL
180808       INCLUDE GFCTB0F3
180808     END-EXEC.

110510     EXEC SQL
110510       INCLUDE GFCTB0M7
110510     END-EXEC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTOV'.

       COPY 'I#GFCTOX'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTOV-ENTRADA
                                                      GFCTOX-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *INICIA O PROGRAMA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTOX-SAIDA.

           INITIALIZE GFCTOX-SAIDA
                      GFCT0M-AREA-ERROS
                      RETURN-CODE.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-VERIFICAR-DISP-SISTEMA.

           PERFORM 1400-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG       TO GFCTOX-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                       TO GFCTOX-LL.
           MOVE ZEROS                      TO GFCTOX-ZZ.
           MOVE GFCTOV-TRANSACAO           TO GFCTOX-TRANSACAO.
           MOVE GFCTOV-FUNCAO              TO GFCTOX-FUNCAO.
           MOVE GFCTOV-FUNC-BDSCO          TO GFCTOX-FUNC-BDSCO.
           MOVE ZEROS                      TO GFCTOX-ERRO
                                              GFCTOX-COD-SQL-ERRO
                                              GFCTOX-COD-MSG-ERRO
                                              GFCTG2-COD-MSG.
           MOVE 'N'                        TO GFCTOX-FIM.

           IF  GFCTOV-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTOX-QTDE-OCOR
           ELSE
               MOVE GFCTOV-QTDE-OCOR       TO GFCTOX-QTDE-OCOR
           END-IF.

           IF  GFCTOV-QTDE-TOT-REG         NOT NUMERIC
               MOVE ZEROS                  TO GFCTOX-QTDE-TOT-REG
           ELSE
               MOVE GFCTOV-QTDE-TOT-REG    TO GFCTOX-QTDE-TOT-REG
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTOV-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOV-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOV-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOV-TIMESTAMP        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTOV-AGRUPMTO         NOT NUMERIC) OR
              (GFCTOV-TARIFA           NOT NUMERIC  OR
               GFCTOV-TARIFA           EQUAL ZEROS) OR
ST2506*       (GFCTOV-CGC-CPF          NOT NUMERIC) OR
ST2506*       (GFCTOV-FILIAL           NOT NUMERIC) OR
ST2506        (GFCTOV-CGC-CPF          NOT EQUAL LOW-VALUES) OR
ST2506        (GFCTOV-FILIAL           NOT EQUAL LOW-VALUES) OR
              (GFCTOV-AGENCIA          NOT NUMERIC) OR
              (GFCTOV-POSTO            NOT NUMERIC) OR
              (GFCTOV-SEGMENTO         NOT NUMERIC) OR
110608        (GFCTOV-MUNICIPIO        NOT NUMERIC) OR
0410          (GFCTOV-GRUPO-CTBIL      NOT NUMERIC) OR
0410          (GFCTOV-SUB-GRUPO-CTBIL  NOT NUMERIC)
110608*       (GFCTOV-AGRUPMTO         NOT EQUAL 4 AND 9 AND 14 AND 15
110608*                                   AND 17 AND 18 AND 19)
               MOVE  1                 TO GFCTOX-ERRO
               MOVE  ZEROS             TO GFCTOX-COD-SQL-ERRO
               MOVE  0001              TO GFCTOX-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTOV-AGRUPMTO

               WHEN 15
                  IF  GFCTOV-AGENCIA       EQUAL ZEROS OR
                      GFCTOV-POSTO         EQUAL ZEROS
                      MOVE 1               TO GFCTOX-ERRO
                      MOVE 0001            TO GFCTOX-COD-MSG-ERRO
                      MOVE '0001'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
                      PERFORM 1400-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 17
                  IF (GFCTOV-AGENCIA       EQUAL ZEROS ) OR
                     (GFCTOV-POSTO         EQUAL ZEROS)
                      MOVE 1               TO GFCTOX-ERRO
                      MOVE 0001            TO GFCTOX-COD-MSG-ERRO
                      MOVE '0001'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
                      PERFORM 1400-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 04
ST2506            INITIALIZE GFCTW2PJ-BLOCO-ENTRADA 
ST2506            MOVE GFCTOV-CGC-CPF       TO GFCTW2PJ-E-CGC-CNPJ 
ST2506            PERFORM 1205-VALIDAR-FORMATO-PJ 

ST2506*           IF (GFCTOV-CGC-CPF       EQUAL ZEROS) OR
ST2506*              (GFCTOV-CONTROLE      EQUAL SPACES OR
ST2506            IF (GFCTOV-CGC-CPF       EQUAL SPACES) OR
ST2506               (GFCTOV-CONTROLE      EQUAL SPACES OR
ST2506                GFCTOV-CONTROLE      EQUAL LOW-VALUES OR
ST2506                GFCTW2PJ-S-INVALIDO)
                      MOVE 1               TO GFCTOX-ERRO
                      MOVE 0001            TO GFCTOX-COD-MSG-ERRO
                      MOVE '0001'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
                      PERFORM 1400-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 09
                  IF  GFCTOV-SEGMENTO      EQUAL ZEROS
                      MOVE 1               TO GFCTOX-ERRO
                      MOVE 0001            TO GFCTOX-COD-MSG-ERRO
                      MOVE '0001'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
                      PERFORM 1400-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 18
                  IF  GFCTOV-MUNICIPIO     EQUAL ZEROS
                      MOVE 1               TO GFCTOX-ERRO
                      MOVE 0001            TO GFCTOX-COD-MSG-ERRO
                      MOVE '0001'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
                      PERFORM 1400-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

               WHEN 19
                  IF  GFCTOV-UF            EQUAL ZEROS
                      MOVE 1               TO GFCTOX-ERRO
                      MOVE 0001            TO GFCTOX-COD-MSG-ERRO
                      MOVE '0001'          TO GFCTG2-COD-MSG
                      MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
                      PERFORM 1400-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF

0410           WHEN 20
0410              IF (GFCTOV-AGENCIA       EQUAL ZEROS ) OR
0410                 (GFCTOV-POSTO         EQUAL ZEROS)
0410                  MOVE 1               TO GFCTOX-ERRO
0410                  MOVE 0001            TO GFCTOX-COD-MSG-ERRO
0410                  MOVE '0001'          TO GFCTG2-COD-MSG
0410                  MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
0410                  PERFORM 1400-OBTER-DESC-MSG
0410                  MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
0410                  PERFORM 3000-FINALIZAR
0410              END-IF
0410
0410           WHEN 26
0410              IF (GFCTOV-GRUPO-CTBIL   EQUAL ZEROS )
0410                  MOVE 1               TO GFCTOX-ERRO
0410                  MOVE 0001            TO GFCTOX-COD-MSG-ERRO
0410                  MOVE '0001'          TO GFCTG2-COD-MSG
0410                  MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
0410                  PERFORM 1400-OBTER-DESC-MSG
0410                  MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
0410                  PERFORM 3000-FINALIZAR
0410              END-IF

           END-EVALUATE.

           PERFORM 1210-SELECIONAR-GFCTB0A1.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
ST2506*----------------------------------------------------------------*
ST2506 1205-VALIDAR-FORMATO-PJ         SECTION.
ST2506*----------------------------------------------------------------*
ST2506  
ST2506     CALL WRK-GFCT20PJ           USING WRK-AREA-GFCT20PJ.
ST2506
ST2506     IF RETURN-CODE NOT EQUAL ZEROS 
ST2506     OR GFCTW2PJ-S-COD-RETORNO NOT EQUAL '00'
ST2506        MOVE 1               TO GFCTOX-ERRO
ST2506        MOVE 0001            TO GFCTOX-COD-MSG-ERRO
ST2506        MOVE '0001'          TO GFCTG2-COD-MSG
ST2506        MOVE ZEROS           TO GFCTOX-COD-SQL-ERRO
ST2506        PERFORM 1400-OBTER-DESC-MSG
ST2506        MOVE GFCTG3-DESC-MSG TO GFCTOX-DESC-MSG-ERRO
ST2506        PERFORM 3000-FINALIZAR
ST2506     END-IF. 
ST2506                                 
ST2506*----------------------------------------------------------------*
ST2506 1205-99-FIM.                    EXIT.
ST2506*----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR DATA ATUAL DA TABELA GFCTB0A1                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-SELECIONAR-GFCTB0A1        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0A1'      TO WRK-NOM-TAB
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE CORR WRK-DATA-AUX      TO WRK-DATA-INV-R.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA ERROS DB2                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1211-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0886'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  9                     TO GFCTOX-ERRO.
           MOVE  SQLCODE               TO GFCTOX-COD-SQL-ERRO
                                          GFCT0M-SQLCODE.
           MOVE SPACES                 TO GFCTOX-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG WRK-NOM-TAB
           DELIMITED BY '  '           INTO GFCTOX-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTOV-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTOV-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE ZEROS                  TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOX-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0886'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
                                              GFCTOX-COD-SQL-ERRO
               MOVE GFCTG1-ERRO            TO GFCTOX-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTOX-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                                              GFCTOX-DESC-MSG-ERRO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTOX-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTOV-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTOV-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTOV-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOX-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTOX-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO GFCTOX-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTOX-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTOX-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-TABELAS.

           IF  CSIT-SOLTC-FLEXZ        OF GFCTB0B2 NOT EQUAL 02 AND 01
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0663               TO GFCTOX-COD-MSG-ERRO
               MOVE '0663'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  CSIT-SOLTC-FLEXZ        OF GFCTB0B2 EQUAL 02
               MOVE DINIC-FLEXZ-AGPTO  OF GFCTB0B2
                                       TO WRK-DATA-AUX
               MOVE WRK-DIA-AUX        OF WRK-DATA-AUX TO WRK-DIA-INV
               MOVE WRK-MES-AUX        OF WRK-DATA-AUX TO WRK-MES-INV
               MOVE WRK-ANO-AUX        OF WRK-DATA-AUX TO WRK-ANO-INV
               IF  WRK-DATA-INV2       GREATER WRK-DATA-INV
                   MOVE 5              TO CSIT-SOLTC-FLEXZ  OF GFCTB0B2
                   PERFORM 2200-EXECUTAR-UPDATE-CANC
               ELSE
                   MOVE 1              TO GFCTOX-ERRO
                   MOVE ZEROS          TO GFCTOX-COD-SQL-ERRO
                   MOVE 0665           TO GFCTOX-COD-MSG-ERRO
                   MOVE '0665'         TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO GFCTOX-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
                END-IF
           END-IF.

           IF  CSIT-SOLTC-FLEXZ        OF GFCTB0B2
                                       EQUAL 01
               MOVE 6                  TO CSIT-SOLTC-FLEXZ OF GFCTB0B2
               PERFORM 2200-EXECUTAR-UPDATE-CANC
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *LER TABELAS                                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-TABELAS                SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOV-AGRUPMTO        TO CAGPTO-CTA    OF GFCTB0B2.
           MOVE GFCTOV-TIMESTAMP       TO HIDTFD-FLEXZ-AGPTO
                                                        OF GFCTB0B2.
           MOVE GFCTOV-TARIFA          TO CSERVC-TARIF  OF GFCTB0B2.

           EXEC SQL
             SELECT
                   CAGPTO_CTA,
                   CSERVC_TARIF,
                   HIDTFD_FLEXZ_AGPTO,
                   CFUNC_SOLTC_FLEXZ,
                   CFUNC_PAREC_FLEXZ,
                   CTPO_OPER_MOTVO,
                   CMOTVO_JUSTF,
                   CEMPR_ORIGE_FLEXZ,
                   CDEPDC_ORIGE_FLEXZ,
                   CEMPR_PAREC_FLEXZ,
                   CDEPDC_PAREC_FLEXZ,
                   DINIC_FLEXZ_AGPTO,
                   DFIM_FLEXZ_AGPTO,
                   QFRANQ_FLEXZ_AGPTO,
                   CPER_FRANQ_AGPTO,
                   VNEGOC_FLEXZ_AGPTO,
                   PDESC_FLEXZ_AGPTO,
                   DDIA_AGNDA,
                   QDIA_FLEXZ_AGPTO,
                   HSOLTC_FLEXZ,
                   RJUSTF_SOLTC_FLEXZ,
                   HPAREC_FLEXZ,
                   RJUSTF_PAREC_FLEXZ,
                   CSIT_SOLTC_FLEXZ,
                   CINDCD_AGPTO_TOT,
                   CINIC_PER_FRANQ,
                   CFUNC_EXCL_SIST
             INTO
                   :GFCTB0B2.CAGPTO-CTA,
                   :GFCTB0B2.CSERVC-TARIF,
                   :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
                   :GFCTB0B2.CFUNC-SOLTC-FLEXZ,
                   :GFCTB0B2.CFUNC-PAREC-FLEXZ,
                   :GFCTB0B2.CTPO-OPER-MOTVO,
                   :GFCTB0B2.CMOTVO-JUSTF,
                   :GFCTB0B2.CEMPR-ORIGE-FLEXZ,
                   :GFCTB0B2.CDEPDC-ORIGE-FLEXZ,
                   :GFCTB0B2.CEMPR-PAREC-FLEXZ,
                   :GFCTB0B2.CDEPDC-PAREC-FLEXZ,
                   :GFCTB0B2.DINIC-FLEXZ-AGPTO,
                   :GFCTB0B2.DFIM-FLEXZ-AGPTO,
                   :GFCTB0B2.QFRANQ-FLEXZ-AGPTO,
                   :GFCTB0B2.CPER-FRANQ-AGPTO,
                   :GFCTB0B2.VNEGOC-FLEXZ-AGPTO,
                   :GFCTB0B2.PDESC-FLEXZ-AGPTO,
                   :GFCTB0B2.DDIA-AGNDA,
                   :GFCTB0B2.QDIA-FLEXZ-AGPTO,
                   :GFCTB0B2.HSOLTC-FLEXZ,
                   :GFCTB0B2.RJUSTF-SOLTC-FLEXZ,
                   :GFCTB0B2.HPAREC-FLEXZ,
                   :GFCTB0B2.RJUSTF-PAREC-FLEXZ,
                   :GFCTB0B2.CSIT-SOLTC-FLEXZ,
                   :GFCTB0B2.CINDCD-AGPTO-TOT,
                   :GFCTB0B2.CINIC-PER-FRANQ,
                   :GFCTB0B2.CFUNC-EXCL-SIST
             FROM   DB2PRD.PDIDO_FLEXZ_AGPTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE GFCTOV-AGRUPMTO
               WHEN 15
                   PERFORM 2110-SELECIONAR-PAB
               WHEN 14
                   IF  CINDCD-AGPTO-TOT    OF GFCTB0B2
                                           NOT EQUAL 'S'
                       PERFORM 2120-SELECIONAR-AGENCIA
                   ELSE
                       MOVE 'TODOS'        TO GFCTOX-DESCR-AGPTO
                       MOVE ZEROS          TO GFCTOX-AGENCIA
                   END-IF
               WHEN 17
                   PERFORM 2130-SELECIONAR-POSTAL
               WHEN 4
                   PERFORM 2140-SELECIONAR-CLIENTE
               WHEN 9
                   PERFORM 2150-SELECIONAR-SEGMENTO
               WHEN 18
                   PERFORM 2160-SELECIONAR-MUNICIPIO
               WHEN 19
                   PERFORM 2170-SELECIONAR-UF
0410           WHEN 20
0410               PERFORM 2130-SELECIONAR-POSTAL
0410           WHEN 26
0410               PERFORM 2190-SELECIONAR-RAZAO
180808         WHEN OTHER
180808             PERFORM 2175-SELECIONAR-AGRUP
           END-EVALUATE.

           PERFORM 2180-PESQUISAR-NOME-TARIFA.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8  TO GFCTOX-NOME-TARIFA.
           MOVE GFCTOV-TARIFA                   TO GFCTOX-TARIFA.
           MOVE GFCTOV-TIMESTAMP                TO GFCTOX-TIMESTAMP.
           MOVE QFRANQ-FLEXZ-AGPTO OF GFCTB0B2  TO GFCTOX-FRANQUIA.
           MOVE CPER-FRANQ-AGPTO   OF GFCTB0B2  TO GFCTOX-PERIODO.
           MOVE VNEGOC-FLEXZ-AGPTO OF GFCTB0B2  TO GFCTOX-VR-NEGOCIADO.
           MOVE PDESC-FLEXZ-AGPTO  OF GFCTB0B2  TO GFCTOX-PERC-DESCTO.
           MOVE QDIA-FLEXZ-AGPTO   OF GFCTB0B2  TO GFCTOX-DIAS-RETENCAO.
           MOVE DDIA-AGNDA         OF GFCTB0B2  TO GFCTOX-DIA-AGEND.
           MOVE DINIC-FLEXZ-AGPTO  OF GFCTB0B2  TO GFCTOX-INIC-VIG-FLEX.
           MOVE DFIM-FLEXZ-AGPTO   OF GFCTB0B2  TO GFCTOX-FIM-VIG-FLEX.
           MOVE GFCTOV-AGRUPMTO                 TO GFCTOX-AGRUPMTO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR PAB                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0B8.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B8.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0B8.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC
             FROM   DB2PRD.PDIDO_FLEXZ_PAB
             WHERE
                   CAGPTO_CTA         = :GFCTB0B8.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B8.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B8'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC                 OF GFCTB0B8
                                       TO GFCTOX-AGENCIA.
           MOVE CPOSTO-SERVC           OF GFCTB0B8
                                       TO GFCTOX-POSTO.

           PERFORM 2111-OBTER-DESC-PAB.

           MOVE GFCTGC-DESC-POSTO(1)   TO GFCTOX-DESCR-AGPTO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO PAB                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTOV-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTOV-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTOV-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB0B8
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB0B8
                                       TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOX-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                 TO GFCTOX-FIM
               MOVE GFCTGC-ERRO         TO GFCTOX-ERRO
               MOVE GFCTGC-COD-SQL-ERRO TO GFCTOX-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO TO GFCTOX-COD-MSG-ERRO
               IF  GFCTGC-ERRO          EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO TO GFCTOX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTOX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR AGENCIA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0B1.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B1.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0B1.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
             FROM   DB2PRD.PDIDO_FLEXZ_AG
             WHERE
                   CAGPTO_CTA         = :GFCTB0B1.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B1.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B1'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2121-OBTER-DESC-AGENCIA.

           MOVE GFCT1X-DESC-DEPDC(1)   TO GFCTOX-DESCR-AGPTO.
           MOVE CDEPDC                 OF GFCTB0B1
                                       TO GFCTOX-AGENCIA.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO AGENCIA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2121-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1V-ENTRADA
                      GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 680                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTOV-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTOV-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTOV-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC OF GFCTB0B1     TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                              GFCT1V-ENTRADA
                                              GFCT1X-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                   TO GFCTOX-ERRO
               MOVE WRK-MODULO          TO GFCT0M-PROGRAMA
                                           WRK-MODULO-MSG
               MOVE WRK-MSG001          TO GFCT0M-TEXTO
               MOVE 'APL'               TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'          TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                 TO GFCTOX-FIM
               MOVE GFCT1X-ERRO         TO GFCTOX-ERRO
               MOVE GFCT1X-COD-SQL-ERRO TO GFCTOX-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO TO GFCTOX-COD-MSG-ERRO
               IF  GFCT1X-ERRO          EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO TO GFCTOX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006 - ' GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE    INTO GFCTOX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR POSTAL                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0B9.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B9.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0B9.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC
             FROM   DB2PRD.PDIDO_FLEXZ_PSTAL
             WHERE
                   CAGPTO_CTA         = :GFCTB0B9.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B9.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B9'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_PSTAL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2131-BUSCAR-DESC-POSTAL.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *BUSCAR DESCRICAO POSTAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-BUSCAR-DESC-POSTAL         SECTION.
      *----------------------------------------------------------------*


           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTOV-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTOV-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE GFCTOV-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB0B9
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB0B9
                                       TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTOX-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0886'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                 TO GFCTOX-FIM
               MOVE GFCTGC-ERRO         TO GFCTOX-ERRO
               MOVE GFCTGC-COD-SQL-ERRO TO GFCTOX-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO TO GFCTOX-COD-MSG-ERRO
               IF  GFCTGC-ERRO          EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO TO GFCTOX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE    INTO GFCTOX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)     TO GFCTOX-DESCR-AGPTO.
           MOVE CDEPDC       OF GFCTB0B9 TO GFCTOX-AGENCIA.
           MOVE CPOSTO-SERVC OF GFCTB0B9 TO GFCTOX-POSTO.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR CLIENTE                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0B3.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B3.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0B3

           EXEC SQL
ST2506*      SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506*      INTO
ST2506*            :GFCTB0B3.CCGC-CPF,
ST2506*            :GFCTB0B3.CFLIAL-CGC,
ST2506*            :GFCTB0B3.CCTRL-CPF-CGC
ST2506       SELECT
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
ST2506       INTO
ST2506             :GFCTB0B3.CCGC-CPF-ST,
ST2506             :GFCTB0B3.CFLIAL-CGC-ST,
ST2506             :GFCTB0B3.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.PDIDO_FLEXZ_CLI
             WHERE
                   CAGPTO_CTA         = :GFCTB0B3.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B3.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B3'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO GFCTOX-DESCR-AGPTO.
ST2506*    MOVE CCGC-CPF               OF GFCTB0B3 TO GFCTOX-CGC-CPF.
ST2506*    MOVE CFLIAL-CGC             OF GFCTB0B3 TO GFCTOX-FILIAL.
ST2506*    MOVE CCTRL-CPF-CGC          OF GFCTB0B3 TO WRK-CONTROLE.
ST2506     MOVE CCGC-CPF-ST            OF GFCTB0B3 TO GFCTOX-CGC-CPF.
ST2506     MOVE CFLIAL-CGC-ST          OF GFCTB0B3 TO GFCTOX-FILIAL.
ST2506     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0B3 TO WRK-CONTROLE.
           MOVE WRK-CONTROLE           TO GFCTOX-CONTROLE.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR SEGMENTO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2150-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0C0.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C0.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0C0.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0C0.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.PDIDO_FLEXZ_SGMTO
             WHERE
                   CAGPTO_CTA         = :GFCTB0C0.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C0.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0C0'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_SGMTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2151-OBTER-DESC-SEGMENTO.

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0C0 TO GFCTOX-SEGMENTO.
           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5 TO GFCTOX-DESCR-AGPTO.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTER DESCRICAO SEGMENTO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2151-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF    OF GFCTB0C0
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOM-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                  EQUAL +100
               MOVE 1                   TO GFCTOX-ERRO
               MOVE ZEROS               TO GFCTOX-COD-SQL-ERRO
               MOVE 0500                TO GFCTOX-COD-MSG-ERRO
               MOVE '0500'              TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG     TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2151-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR MUNICIPIO                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2160-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0B7.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0B7.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0B7.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB0B7.CMUN-IBGE
             FROM   DB2PRD.PDIDO_FLEXZ_MUN
             WHERE
                   CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B7'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_MUN'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2161-BUSCAR-DESC-MUN.

      *----------------------------------------------------------------*
       2160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *BUSCAR DESCRICAO MUNICIPIO                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2161-BUSCAR-DESC-MUN            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB0B7
                                       TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                              AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0732               TO GFCTOX-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0675               TO GFCTOX-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0733               TO GFCTOX-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTOX-ERRO
               MOVE SQLCODE-RURC88     TO GFCTOX-COD-SQL-ERRO
               MOVE 0734               TO GFCTOX-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CMUN-IBGE OF GFCTB0B7  TO GFCTOX-MUNICIPIO.
           MOVE NOME-MUNIC-RURC88      TO GFCTOX-DESCR-AGPTO.

      *----------------------------------------------------------------*
       2161-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *SELECIONAR UF                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2170-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF      OF GFCTB0C1.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
                                      TO HIDTFD-FLEXZ-AGPTO OF GFCTB0C1.
           MOVE CAGPTO-CTA             OF GFCTB0B2
                                       TO CAGPTO-CTA        OF GFCTB0C1.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB0C1.CSGL-UF
             FROM   DB2PRD.PDIDO_FLEXZ_UF
             WHERE
                   CAGPTO_CTA         = :GFCTB0C1.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0C1.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0C1'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_UF'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
               MOVE 0926               TO GFCTOX-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2171-BUSCAR-DESC-UF.

      *----------------------------------------------------------------*
       2170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *BUSCAR DESCRICAO UF                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2171-BUSCAR-DESC-UF             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE DCITV002.

           MOVE CSGL-UF OF GFCTB0C1    TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA-DCIT.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0735               TO GFCTOX-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0736               TO GFCTOX-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0737               TO GFCTOX-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTOX-ERRO
               MOVE ZEROS              TO GFCTOX-COD-SQL-ERRO
               MOVE 0738               TO GFCTOX-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'DCITV002'         TO WRK-NOM-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTOX-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTOX-ERRO
               MOVE 'GFCT0886'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA-DCIT     TO SQLCA
               MOVE WRK-SQLCA-DCIT     TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTOX-COD-SQL-ERRO
               MOVE SPACES             TO GFCTOX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSGL-UF     OF GFCTB0C1    TO GFCTOX-UF.
           MOVE WRK-AREA-DCLGEN-DCITV002   TO DCITV002.
           MOVE IUF OF DCITV002            TO GFCTOX-DESCR-AGPTO.

      *----------------------------------------------------------------*
       2171-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

180808******************************************************************
180808*PESQUISAR NOME DO AGRUPAMENTO PERSONALIZADO                     *
180808******************************************************************
180808*----------------------------------------------------------------*
180808 2175-SELECIONAR-AGRUP           SECTION.
180808*----------------------------------------------------------------*
180808
180808     MOVE CAGPTO-CTA             OF GFCTB0B2
180808                                 TO CAGPTO-CTA OF GFCTB0F3.
180808
180808     EXEC SQL
180808       SELECT  RAGPTO_CTA
180808         INTO  :GFCTB0F3.RAGPTO-CTA
180808         FROM  DB2PRD.TPO_AGPTO_CTA
180808        WHERE  CAGPTO_CTA         = :GFCTB0F3.CAGPTO-CTA
180808     END-EXEC.
180808
180808     IF (SQLCODE                 NOT EQUAL ZEROS
180808                                 AND +100 AND -811) OR
180808        (SQLWARN0                EQUAL 'W')
180808         INITIALIZE              GFCT0M-ERRO-SQL
180808         MOVE '0010'             TO GFCTG2-COD-MSG
180808         PERFORM 1400-OBTER-DESC-MSG
180808         MOVE ' - GFCTB0F3'      TO WRK-NOM-TAB
180808         MOVE 'TPO_AGPTO_CTA'    TO GFCT0M-NOME-TAB
180808         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
180808         MOVE '0120'             TO GFCT0M-LOCAL
180808         MOVE  0010              TO GFCTOX-COD-MSG-ERRO
180808         PERFORM 1211-FORMATAR-ERRO-DB2
180808         PERFORM 3000-FINALIZAR
180808     END-IF.
180808
180808     IF  SQLCODE                 EQUAL +100
180808         MOVE 'AGRUPAMENTO NAO ENCONTRADO'
180808                                 TO RAGPTO-CTA OF GFCTB0F3
180808     END-IF.
180808
180808     MOVE RAGPTO-CTA             OF GFCTB0F3
180808                                 TO GFCTOX-DESCR-AGPTO.
180808
180808*----------------------------------------------------------------*
180808 2175-99-FIM.                    EXIT.
180808*----------------------------------------------------------------*

      ******************************************************************
      *PESQUISAR NOME TARIFA                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2180-PESQUISAR-NOME-TARIFA      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0B2
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOM-TAB
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0410  ******************************************************************
0410  *SELECIONAR RAZAO                                                *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   2190-SELECIONAR-RAZAO           SECTION.
0410  *----------------------------------------------------------------*
0410
0410       MOVE CSERVC-TARIF           OF GFCTB0B2
0410                                   TO CSERVC-TARIF       OF GFCTB0M7
0410       MOVE CAGPTO-CTA             OF GFCTB0B2
0410                                   TO CAGPTO-CTA         OF GFCTB0M7
0410       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB0B2
0410                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M7
0410
0410       EXEC SQL
0410         SELECT
0410               CGRP_CTBIL_ATUAL,
0410               CSGRP_CTBIL
0410         INTO
0410               :GFCTB0M7.CGRP-CTBIL-ATUAL,
0410               :GFCTB0M7.CSGRP-CTBIL
0410         FROM   DB2PRD.TPDIDO_FLEXZ_RZ
0410         WHERE
0410                CAGPTO_CTA         = :GFCTB0M7.CAGPTO-CTA
0410           AND  CSERVC_TARIF       = :GFCTB0M7.CSERVC-TARIF
0410           AND  HIDTFD_FLEXZ_AGPTO = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0410       END-EXEC.
0410
0410       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0410          (SQLWARN0                EQUAL 'W')
0410           INITIALIZE GFCT0M-ERRO-SQL
0410           MOVE '0010'             TO GFCTG2-COD-MSG
0410           PERFORM 1400-OBTER-DESC-MSG
0410           MOVE ' - GFCTB0M7'      TO WRK-NOM-TAB
0410           MOVE 'TPDIDO_FLEXZ_RZ ' TO GFCT0M-NOME-TAB
0410           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0410           MOVE '0140'             TO GFCT0M-LOCAL
0410           MOVE  0010              TO GFCTOX-COD-MSG-ERRO
0410           PERFORM 1211-FORMATAR-ERRO-DB2
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410       IF  SQLCODE                 EQUAL +100
0410           MOVE 1                  TO GFCTOX-ERRO
0410           MOVE ZERO               TO GFCTOX-COD-SQL-ERRO
0410           MOVE 0926               TO GFCTOX-COD-MSG-ERRO
0410           MOVE '0926'             TO GFCTG2-COD-MSG
0410           PERFORM 1400-OBTER-DESC-MSG
0410           MOVE GFCTG3-DESC-MSG    TO GFCTOX-DESC-MSG-ERRO
0410           PERFORM 3000-FINALIZAR
0410       END-IF.

0410       MOVE CGRP-CTBIL-ATUAL OF GFCTB0M7  TO GFCTOX-GRUPO-CTBIL
0410       MOVE CSGRP-CTBIL      OF GFCTB0M7  TO GFCTOX-SUB-GRUPO-CTBIL.
0410
0410  *----------------------------------------------------------------*
0410   2190-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*
      ******************************************************************
      *EXECUTAR UPDATE DE CANCELAMENTO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-EXECUTAR-UPDATE-CANC       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE   DB2PRD.PDIDO_FLEXZ_AGPTO
               SET
                   CSIT_SOLTC_FLEXZ   = :GFCTB0B2.CSIT-SOLTC-FLEXZ
             WHERE
                   CAGPTO_CTA         = :GFCTB0B2.CAGPTO-CTA         AND
                   CSERVC_TARIF       = :GFCTB0B2.CSERVC-TARIF       AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B2.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE ' - GFCTB0B2'      TO WRK-NOM-TAB
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTOX-COD-MSG-ERRO
               PERFORM 1211-FORMATAR-ERRO-DB2
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *RETORNA AO CHAMADOR                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
