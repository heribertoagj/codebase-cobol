      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5505.
       AUTHOR. FERNANDO BOTELHO.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5505                                     *
      *    PROGRAMADOR  : FERNANDO BOTELHO            - CPM/FPOLIS     *
      *    ANALISTA CPM : RODRIGO WILLAIM             - CPM/FPOLIS     *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK  - GRUPO 50       *
      *    DATA         : 24/03/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      OBTER CODIGO DE JUNCAO (SENH0315).                        *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL1050 - CONEXAO COM DB2.                               *
      *      SENH0315 - OBTER DADOS DO FUNCIONARIO                     *
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
           '*  INICIO DA WORKING GFCT5505  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-BDSCO               PIC X(51)           VALUE
           'CODIGO NAO E DE FUNCIONARIO DA ORGANIZACAO BRADESCO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(06)          VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC  9(03)          VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(79)          VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(07)          VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(40)          VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(05)          VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(40)          VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     05  WRK-CNPJ-PRI-0315       PIC  9(09)          VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-FIL-0315       PIC  9(05)          VALUE ZEROS.
ST25X6*     05  WRK-CNPJ-CON-0315       PIC  9(02)          VALUE ZEROS.
           05  WRK-CNPJ-PRI-0315       PIC  X(09)          VALUE SPACES.
           05  WRK-CNPJ-FIL-0315       PIC  X(04)          VALUE SPACES.
           05  WRK-CNPJ-CON-0315       PIC  X(02)          VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(05)          VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(40)          VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(03)          VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(01)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT5505   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LKG-5505-AREA-ENTRADA.
           03  LKG-5505-AMBIENTE       PIC X(01).
           03  LKG-5505-FUNC-BDSCO     PIC X(07).

       01  LKG-5505-AREA-SAIDA.
           03  LKG-5505-COD-RETORNO    PIC 9(02).
           03  LKG-5505-COD-SQL-ERRO   PIC 9(03).
           03  LKG-5505-DESC-MSG       PIC X(70).
           03  LKG-5505-DADOS-RETORNO.
               05  LKG-5505-COD-JUNCAO PIC 9(05).

       01  LKG-IO-PCB.
           03  LKG-IO-LTERM            PIC X(08).
           03  FILLER                  PIC X(02).
           03  LKG-IO-STATUS           PIC X(02).
           03  FILLER                  PIC X(12).
           03  LKG-IO-MODNAME          PIC X(08).

       01  LKG-ALT-PCB.
           03  LKG-ALT-LTERM           PIC X(08).
           03  FILLER                  PIC X(02).
           03  LKG-ALT-STATUS          PIC X(02).
           03  FILLER                  PIC X(12).
           03  LKG-ALT-MODNAME         PIC X(08).

      *================================================================*
       PROCEDURE DIVISION              USING LKG-5505-AREA-ENTRADA
                                             LKG-5505-AREA-SAIDA
                                             LKG-IO-PCB
                                             LKG-ALT-PCB.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE LKG-5505-AREA-SAIDA.

           IF  LKG-5505-AMBIENTE       NOT EQUAL 'B' AND 'O'
               MOVE 01                 TO LKG-5505-COD-RETORNO
               MOVE 'AMBIENTE DIFERENTE DE B/O'
                                       TO LKG-5505-DESC-MSG
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  LKG-5505-AMBIENTE       EQUAL 'B'
               CALL 'POOL1050'
           END-IF.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE LKG-5505-FUNC-BDSCO    TO WRK-COD-FUNCIONAL-0315.
           MOVE 'VRS001'               TO WRK-VERSAO-0315.

           CALL 'SENH0315'             USING WRK-VERSAO-0315
                                             WRK-MENSAGEM-0315
                                             WRK-COD-FUNCIONAL-0315
                                             WRK-NOME-FUNC-0315
                                             WRK-COD-BANCO-0315
                                             WRK-NOME-BANCO-0315
                                             WRK-CNPJ-BANCO-0315
                                             WRK-COD-JUNCAO-0315
                                             WRK-NOME-DEPTO-0315
                                             WRK-SECAO-0315
                                             WRK-STATUS-0315.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 99                 TO LKG-5505-COD-RETORNO
               MOVE RETURN-CODE        TO LKG-5505-COD-SQL-ERRO
               MOVE 'ERRO NO MODULO GFCT5505 - ACESSO SENH0315'
                                       TO LKG-5505-DESC-MSG
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL ZEROS
               IF  WRK-COD-MSG-0315    EQUAL '009'
                   PERFORM 21000-FORMATAR-RETORNO
                   MOVE ZEROS          TO LKG-5505-COD-RETORNO
                   MOVE ZEROS          TO LKG-5505-COD-SQL-ERRO
                   MOVE 'PROCESSO EFETUADO COM SUCESSO'
                                       TO LKG-5505-DESC-MSG
                   PERFORM 30000-FINALIZAR
               ELSE
                   IF  WRK-COD-MSG-0315
                                       EQUAL '138'
                       MOVE 05         TO LKG-5505-COD-RETORNO
                       MOVE ZEROS      TO LKG-5505-COD-SQL-ERRO
                       MOVE 'USUARIO NAO ENCONTRADO'
                                       TO LKG-5505-DESC-MSG
                       PERFORM 30000-FINALIZAR
                   END-IF
               END-IF
           END-IF.

           IF  WRK-COD-MSG-0315        EQUAL '001'
               MOVE 02                 TO LKG-5505-COD-RETORNO
               MOVE ZEROS              TO LKG-5505-COD-SQL-ERRO
               MOVE 'CODIGO DO USUARIO INCONSISTENTE'
                                       TO LKG-5505-DESC-MSG
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-COD-MSG-0315        EQUAL '154'
               MOVE 03                 TO LKG-5505-COD-RETORNO
               MOVE ZEROS              TO LKG-5505-COD-SQL-ERRO
               MOVE WRK-MSG-BDSCO      TO LKG-5505-DESC-MSG
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-COD-MSG-0315        EQUAL '242'
               MOVE 04                 TO LKG-5505-COD-RETORNO
               MOVE ZEROS              TO LKG-5505-COD-SQL-ERRO
               MOVE 'VERSAO INCONSISTENTE'
                                       TO LKG-5505-DESC-MSG
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-FORMATAR-RETORNO          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-JUNCAO-0315    TO LKG-5505-COD-JUNCAO.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
