      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5110.
       AUTHOR.     PROCWORK OUTSOURCING.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5110                                     *
      *    PROGRAMADOR  : PROCWORK OUTSOURCING                         *
      *    ANALISTA PWO : MARCUS VINICIUS    - PROCWORK   - GRUPO 50   *
      *    ANALISTA     : FRANCISCO LOURENCO - PROCWORK   - GRUPO 50   *
      *    DATA         : 23/12/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      OBTER NOME DO CLIENTE POR CPF/CNPJ                        *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *      DB2PRD.V01CLIENTE_AGENCIA               CLIEV004          *
      *      DB2PRD.V01CTA_POUPANCA                  CLIEV007          *
      *      DB2PRD.V01CTA_CORRENTE                  CLIEV008          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      POOL0110 - CONSISTENCIA CPF/CNPJ.
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5110  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    VARIAVEIS ACUMULADORAS    *'.
      *----------------------------------------------------------------*

       01  WRK-AC-LIDOS-CLIEV004       PIC 9(05)           VALUE ZEROS.
       01  WRK-AC-LIDOS-CLIEV007       PIC 9(05)           VALUE ZEROS.
       01  WRK-AC-LIDOS-CLIEV008       PIC 9(05)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TABELA             PIC X(20)           VALUE SPACES.
       01  WRK-COD-TABELA              PIC X(08)           VALUE SPACES.

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-NOME-MODULO         PIC X(08)           VALUE SPACES.

ST2506*---------------------------------------------------------------*
ST2506*01  FILLER                      PIC  X(050)         VALUE
ST2506*    '*** AREA PARA CHAVE DE ACESSO A POOL0110 ***'.
ST2506*---------------------------------------------------------------*
ST2506*
ST2506*01  WRK-0110-CPF-CNPJ.
ST2506*    03  WRK-0110-NRO-CPF-CNPJ   PIC  9(009)         VALUE ZEROS.
ST2506*    03  WRK-0110-NRO-FILIAL     PIC  9(005)         VALUE ZEROS.
ST2506*
ST2506*01  WRK-0110-CONTROLE.
ST2506*    03  WRK-0110-CTRL-CPF-CNPJ  PIC  9(002)         VALUE ZEROS.
ST2506*
ST2506*----------------------------------------------------------------*
ST2506 01 WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506       'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
ST2506
ST2506*------------------------------------*----------------------------*
ST2506 01 FILLER                       PIC  X(051) VALUE
ST2506       '* AREA PARA MODULO GFCT30PJ *'.
ST2506*---------------------------------------------------------------*
ST2506 01 WRK-GFCT20PJ                 PIC X(08)   VALUE
ST2506       'GFCT20PJ'.
ST2506
ST2506 01 WRK-AREA-GFCT20PJ.
ST2506 COPY 'GFCTW2PJ'.
ST2506

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE TABELAS DB2      *'.
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

           EXEC SQL
                DECLARE  CSR-CLIEV004 CURSOR FOR
                 SELECT  CID_CLI
                   FROM  DB2PRD.V01CLIENTE_AGENCIA
                  WHERE  CCGC_CPF       = :CLIEV004.CCGC-CPF
                    AND  CFLIAL_CGC     = :CLIEV004.CFLIAL-CGC
                    AND  CCTRL_CPF_CGC  = :CLIEV004.CCTRL-CPF-CGC
               ORDER BY  CID_CLI
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5110   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.
       COPY 'I#GFCTIV'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
ST2506*----------------------------------------------------------------*
ST2506 1000-VALIDAR-FORMATO-PJ         SECTION.
ST2506*----------------------------------------------------------------*
ST2506  
ST2506     CALL WRK-GFCT20PJ           USING WRK-AREA-GFCT20PJ.
ST2506
ST2506     IF RETURN-CODE              NOT EQUAL ZEROS 
ST2506     OR GFCTW2PJ-S-COD-RETORNO   NOT EQUAL '00'
ST2506        MOVE 'S'            TO GFCTIV-FIM
ST2506        MOVE 1              TO GFCTIV-ERRO
ST2506        MOVE ZEROS          TO GFCTIV-COD-SQL-ERRO
ST2506        MOVE 0760           TO GFCTIV-COD-MSG-ERRO
ST2506                               GFCTG2-COD-MSG
ST2506        PERFORM 11100-OBTER-DESC-MENSAGEM
ST2506        MOVE GFCTG3-DESC-MSG
ST2506                            TO GFCTIV-DESC-MSG-ERRO
ST2506        PERFORM 30000-FINALIZAR           
ST2506     END-IF. 
ST2506                                 
ST2506*----------------------------------------------------------------*
ST2506 1000-99-FIM.                    EXIT.
ST2506*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZAR-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZAR-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIV-SAIDA.
           INITIALIZE                  GFCTIV-SAIDA.

           MOVE 1010                   TO GFCTIV-LL.
           MOVE GFCTIU-ZZ              TO GFCTIV-ZZ.
           MOVE GFCTIU-TRANSACAO       TO GFCTIV-TRANSACAO.
           MOVE GFCTIU-FUNCAO          TO GFCTIV-FUNCAO.

           IF  GFCTIU-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTIV-QTDE-OCOR
           ELSE
               MOVE GFCTIU-QTDE-OCOR   TO GFCTIV-QTDE-OCOR
           END-IF.

           IF  GFCTIU-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTIV-QTDE-TOT-REG
           ELSE
               MOVE GFCTIU-QTDE-TOT-REG
                                       TO GFCTIV-QTDE-TOT-REG
           END-IF.

           MOVE GFCTIU-FUNC-BDSCO      TO GFCTIV-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIV-FIM.

ST2506*    MOVE GFCTIU-NRO-CPF-CNPJ    TO GFCTIV-NRO-CPF-CNPJ.
ST2506*    MOVE GFCTIU-NRO-FILIAL      TO GFCTIV-NRO-FILIAL.
ST2506     MOVE GFCTIU-COD-CPF-CNPJ    TO GFCTIV-COD-CPF-CNPJ.
ST2506     MOVE GFCTIU-COD-FILIAL      TO GFCTIV-COD-FILIAL.
           MOVE GFCTIU-CTRL-CPF-CNPJ   TO GFCTIV-CTRL-CPF-CNPJ.

           MOVE ZEROS                  TO GFCTIV-ERRO
                                          GFCTIV-COD-SQL-ERRO
                                          GFCTIV-COD-MSG-ERRO.

           MOVE ZEROS                  TO GFCTG2-COD-MSG.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTIV-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*


           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.

           MOVE GFCTIU-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTIU-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTIU-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE SPACES                 TO GFCTG3-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTG3-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTIV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5110'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTIV-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5110'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTIV-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTIV-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTIV-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTIV-DESC-MSG-ERRO

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIU-TRANSACAO        EQUAL SPACES  OR
               GFCTIU-FUNCAO           EQUAL SPACES  OR
               GFCTIU-QTDE-OCOR        NOT NUMERIC   OR
               GFCTIU-QTDE-TOT-REG     NOT NUMERIC   OR
               GFCTIU-FUNC-BDSCO       EQUAL SPACES  OR
ST2506*        GFCTIU-NRO-CPF-CNPJ     NOT NUMERIC   OR
ST2506*        GFCTIU-NRO-FILIAL       NOT NUMERIC   OR
ST2506*        GFCTIU-COD-CPF-CNPJ     EQUAL LOW-VALUES OR
ST2506*        GFCTIU-COD-FILIAL       EQUAL LOW-VALUES OR
ST2506         GFCTIU-COD-CPF-CNPJ     EQUAL SPACES OR
ST2506         GFCTIU-COD-FILIAL       EQUAL SPACES OR
ST2506*        GFCTIU-CTRL-CPF-CNPJ    NOT NUMERIC   OR
ST2506         GFCTIU-CTRL-CPF-CNPJ    NOT NUMERIC   OR
              (GFCTIU-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTIV-ERRO
               MOVE ZEROS              TO GFCTIV-COD-SQL-ERRO
               MOVE 0001               TO GFCTIV-COD-MSG-ERRO
               MOVE 'S'                TO GFCTIV-FIM

               MOVE 0001               TO GFCTG2-COD-MSG

               PERFORM 11100-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO GFCTIV-DESC-MSG-ERRO

               PERFORM 30000-FINALIZAR
           END-IF.
      *
      *--- VALIDAR CPF/CNPJ
      *
           PERFORM 12100-VALIDAR-CPF-CNPJ.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12100-VALIDAR-CPF-CNPJ          SECTION.
      *----------------------------------------------------------------*
           
ST2506     MOVE GFCTIU-COD-CPF-CNPJ         TO GFCTW2PJ-E-CGC-CNPJ 
ST2506     MOVE GFCTIU-COD-FILIAL           TO GFCTW2PJ-E-FILIAL 
           PERFORM 1000-VALIDAR-FORMATO-PJ.

ST2506*    MOVE GFCTIU-NRO-CPF-CNPJ    TO WRK-0110-NRO-CPF-CNPJ.
ST2506*    MOVE GFCTIU-NRO-FILIAL      TO WRK-0110-NRO-FILIAL.
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-0110-CPF-CNPJ
ST2506*                                      WRK-0110-CONTROLE.
ST2506*
ST2506*    IF  RETURN-CODE             EQUAL ZEROS
ST2506*        IF  WRK-0110-CTRL-CPF-CNPJ
ST2506*                                NOT EQUAL GFCTIU-CTRL-CPF-CNPJ
ST2506*            MOVE 'S'            TO GFCTIV-FIM
ST2506*            MOVE 1              TO GFCTIV-ERRO
ST2506*            MOVE ZEROS          TO GFCTIV-COD-SQL-ERRO
ST2506*            MOVE 0760           TO GFCTIV-COD-MSG-ERRO
ST2506*                                   GFCTG2-COD-MSG
ST2506*
ST2506*            PERFORM 11100-OBTER-DESC-MENSAGEM
ST2506*
ST2506*            MOVE GFCTG3-DESC-MSG
ST2506*                                TO GFCTIV-DESC-MSG-ERRO
ST2506*
ST2506*            PERFORM 30000-FINALIZAR
ST2506*        END-IF

ST2506     MOVE ZEROS          TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTIU-COD-CPF-CNPJ 
                               TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTIU-COD-FILIAL
                               TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
ST2506                                 WRK-CAD-CONTROLE.
ST2506     IF  RETURN-CODE             EQUAL ZEROS
ST2506         IF  WRK-CAD-CONTROLE    NOT EQUAL GFCTIU-CTRL-CPF-CNPJ
ST2506             MOVE 'S'            TO GFCTIV-FIM
ST2506             MOVE 1              TO GFCTIV-ERRO
ST2506             MOVE ZEROS          TO GFCTIV-COD-SQL-ERRO
ST2506             MOVE 0760           TO GFCTIV-COD-MSG-ERRO
ST2506                                    GFCTG2-COD-MSG
ST2506  
ST2506             PERFORM 11100-OBTER-DESC-MENSAGEM
ST2506  
ST2506             MOVE GFCTG3-DESC-MSG
ST2506                                 TO GFCTIV-DESC-MSG-ERRO
ST2506  
ST2506             PERFORM 30000-FINALIZAR
ST2506         END-IF
           ELSE
               MOVE 'S'                TO GFCTIV-FIM
               MOVE 1                  TO GFCTIV-ERRO
               MOVE ZEROS              TO GFCTIV-COD-SQL-ERRO

               EVALUATE RETURN-CODE
                   WHEN 04
                        MOVE 0757      TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   WHEN 08
                        MOVE 0758      TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   WHEN 12
                        MOVE 0759      TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               END-EVALUATE

               PERFORM 11100-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO GFCTIV-DESC-MSG-ERRO

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-TRATAR-CSR-CLIEV004.

           PERFORM 22000-TRATAR-FIM-CSR-CLIEV004.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-TRATAR-CSR-CLIEV004       SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE GFCTIU-NRO-CPF-CNPJ    TO CCGC-CPF      OF CLIEV004.
ST2506*    MOVE GFCTIU-NRO-FILIAL      TO CFLIAL-CGC    OF CLIEV004.
ST2506     MOVE GFCTIU-COD-CPF-CNPJ    TO CCGC-CPF      OF CLIEV004.
ST2506     MOVE GFCTIU-COD-FILIAL      TO CFLIAL-CGC    OF CLIEV004.
           MOVE GFCTIU-CTRL-CPF-CNPJ   TO CCTRL-CPF-CGC OF CLIEV004.

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 21100-ABRIR-CSR-CLIEV004.

           PERFORM 21200-LER-CSR-CLIEV004.

           PERFORM
             UNTIL WRK-FINALIZAR       EQUAL 'S'
               PERFORM 21400-TRATAR-COD-CLIENTE

               PERFORM 21200-LER-CSR-CLIEV004
           END-PERFORM.

           PERFORM 21300-FECHAR-CSR-CLIEV004.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21100-ABRIR-CSR-CLIEV004        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO WRK-NOME-TABELA
               MOVE 'CLIEV004'         TO WRK-COD-TABELA

               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 21500-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       21100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21200-LER-CSR-CLIEV004          SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-CLIEV004
               INTO   :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO WRK-NOME-TABELA
               MOVE 'CLIEV004'         TO WRK-COD-TABELA

               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 21500-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
           ELSE
               ADD 1                   TO WRK-AC-LIDOS-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       21200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21300-FECHAR-CSR-CLIEV004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE   CSR-CLIEV004
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO WRK-NOME-TABELA
               MOVE 'CLIEV004'         TO WRK-COD-TABELA

               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 21500-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       21300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21400-TRATAR-COD-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           PERFORM 21410-ACESSAR-CLIEV008.

           IF  SQLCODE                 EQUAL +100
               PERFORM 21420-ACESSAR-CLIEV007
           END-IF.

      *----------------------------------------------------------------*
       21400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21410-ACESSAR-CLIEV008          SECTION.
      *----------------------------------------------------------------*

           MOVE CID-CLI OF CLIEV004    TO CID-CLI OF CLIEV008.

           EXEC SQL
               SELECT  ICTA_MOVTC
               INTO   :CLIEV008.ICTA-MOVTC
               FROM    DB2PRD.V01CTA_CORRENTE
               WHERE   CID_CLI         = :CLIEV008.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811
                                                       AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'V01CTA_CORRENTE'  TO WRK-NOME-TABELA
               MOVE 'CLIEV008'         TO WRK-COD-TABELA

               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 21500-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               ADD 1                   TO WRK-AC-LIDOS-CLIEV008

               IF  GFCTIV-NOME-CLIENTE EQUAL SPACES
                   MOVE ICTA-MOVTC OF CLIEV008
                                       TO GFCTIV-NOME-CLIENTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21410-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21420-ACESSAR-CLIEV007          SECTION.
      *----------------------------------------------------------------*

           MOVE CID-CLI OF CLIEV004    TO CID-CLI OF CLIEV007.

           EXEC SQL
               SELECT  ICTA_MOVTC
               INTO   :CLIEV007.ICTA-MOVTC
               FROM    DB2PRD.V01CTA_POUPANCA
               WHERE   CID_CLI         = :CLIEV007.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811
                                                       AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'V01CTA_POUPANCA'  TO WRK-NOME-TABELA
               MOVE 'CLIEV007'         TO WRK-COD-TABELA

               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 21500-MOVER-ERRO-SQL
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS OR -811
               ADD 1                   TO WRK-AC-LIDOS-CLIEV007

               IF  GFCTIV-NOME-CLIENTE EQUAL SPACES
                   MOVE ICTA-MOVTC OF CLIEV007
                                       TO GFCTIV-NOME-CLIENTE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21420-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21500-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTIV-ERRO.
           MOVE 'GFCT5110'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE WRK-NOME-TABELA        TO GFCT0M-NOME-TAB.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTIV-COD-SQL-ERRO.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           STRING '(GFCT5110) '
                  GFCTG3-DESC-MSG
                  ' - '
                  WRK-COD-TABELA
           DELIMITED BY '  '           INTO GFCTIV-DESC-MSG-ERRO.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       21500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-TRATAR-FIM-CSR-CLIEV004   SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO GFCTIV-FIM.

           IF  WRK-AC-LIDOS-CLIEV004   EQUAL ZEROS OR
              (WRK-AC-LIDOS-CLIEV007   EQUAL ZEROS AND
               WRK-AC-LIDOS-CLIEV008   EQUAL ZEROS)
               MOVE 1                  TO GFCTIV-ERRO
               MOVE ZEROS              TO GFCTIV-COD-SQL-ERRO
               MOVE 0506               TO GFCTIV-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

               PERFORM 11100-OBTER-DESC-MENSAGEM

               MOVE GFCTG3-DESC-MSG    TO GFCTIV-DESC-MSG-ERRO

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
