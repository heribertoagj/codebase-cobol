      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0350.

       AUTHOR.     HOMI.
      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0350                                    *
      *    PROGRAMADOR.:   HOMI                                        *
      *    ANALISTA....:   PAGNOCCA - SONDA IT - GR. 50                *
      *    DATA........:   OUT/2015                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA INVESTIMENTOS ULTIMOS 03 MESES     *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.TPOSIC_SDO_APLIC           GFCTB0P0              *
      *                                                                *
      *    BOOK'S...........:                                          *
      *        GFCTWF19 - ENT - AREA DE COMUNICACAO                    *
      *        GFCTWF20 - SAI - AREA DE COMUNICACAO                    *
      *        I#GFCTFZ - ENT - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG1 - SAI - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG2 - ENT - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG3 - SAI - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCT0M - AREA DE COMUNICACAO - ERROS.                 *
      *        I#CLIB20 - AREA DO CLIB1000                             *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.        *
      *        GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                *
      *        CLIB1000 - OBTER DADOS CADASTRAIS                       *
      *                                                                *
      *    NAVEGACAO                                                   *
      *        CHAMADO POR: GFCT7327                                   *
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
       77  FILLER                      PIC  X(079)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(079)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  IND-1                       PIC  9(001) COMP-3  VALUE ZEROS.
       01  WRK-FIM-GFCTB0P0            PIC  X(001)         VALUE SPACES.

       01  WRK-S9-9                    PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-9.
           05  WRK-9-9                 PIC  9(009).
           05  FILLER                  REDEFINES WRK-9-9.
               07  FILLER              PIC  9(002).
               07  WRK-9-7             PIC  9(007).
           05  FILLER                  REDEFINES WRK-9-9.
               07  FILLER              PIC  9(005).
               07  WRK-9-4             PIC  9(004).

       01  WRK-X-2                     PIC  X(002)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-X-2.
           05  WRK-9-2                 PIC  9(002).

       01  WRK-MSG-AUX.
           05 WRK-MSG-75               PIC  X(075)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE SPACES.

       01  WRK-S9-3                    PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S9-3.
           05  FILLER                  PIC  X(007).
           05  WRK-9-3                 PIC  9(003).

       01  WRK-VSDO-CS                 PIC +9(015)V99 VALUE ZEROS.
       01  REDEFINES WRK-VSDO-CS.
           05                          PIC  X.
           05 WRK-VSDO-SS              PIC  9(015)V99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           'AREA PARA MENSAGENS DE ERRO'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA AO MODULO '.
           05  FILLER                  PIC  X(003)         VALUE ' - '.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.
       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           'AREA DE COMUNICACAO COM GFCT5523'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CLIB1000 ***'.
      *---------------------------------------------------------------*

       COPY 'I#CLIB20'.

       01  WRK-AREA-ENTRADA.
           03  WRK-TRANSACAO       PIC 9(05) COMP-3 VALUE 00001.
           03  WRK-VERSAO          PIC X(06)        VALUE 'VRS000'.
           03  WRK-PRODUTO         PIC 9(05) COMP-3 VALUE 00001.
           03  WRK-AREA-TRABALHO.
               05  WRK-BANCO       PIC 9(05) COMP-3 VALUE 00237.
               05  WRK-AGENCIA     PIC 9(05) COMP-3 VALUE ZEROS.
               05  WRK-CONTA       PIC 9(13) COMP-3 VALUE ZEROS.
               05  WRK-FILLER      PIC X(95)        VALUE SPACES.

       01  WRK-AREA-SAIDA.
           03  WRK-CODIGO-RETORNO  PIC 9(03) COMP-3 VALUE ZEROS.
           03  WRK-MENSAGEM-SAIDA  PIC X(998)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           'AREA PARA TABELA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL INCLUDE SQLCA    END-EXEC.

           EXEC SQL INCLUDE GFCTB0P0 END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0P0 CURSOR FOR
                SELECT DANO_MES_SDO
                  FROM DB2PRD.TPOSIC_SDO_APLIC
                 WHERE CBCO         = :GFCTB0P0.CBCO
                   AND CAG_BCRIA    = :GFCTB0P0.CAG-BCRIA
                   AND CCTA_CLI     = :GFCTB0P0.CCTA-CLI
                   AND DANO_MES_SDO > :GFCTB0P0.DANO-MES-SDO
              ORDER BY DANO_MES_SDO DESC
                 FETCH FIRST 3 ROWS ONLY
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(079)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWF19'.

       COPY 'GFCTWF20'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING WF19-ENTRADA
                                                      WF20-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    ROTINA INICIAL DO PROCESSAMENTO.                            *
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
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          WF20-SAIDA.
           INITIALIZE                     GFCT0M-AREA-ERROS
                                          WF20-SAIDA.

           PERFORM 1100-INICIALIZA-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1300-CONSISTENCIA-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA AREA DE SAIDA.                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-INICIALIZA-AREA-SAIDA      SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO WF20-LL.
           MOVE ZEROS                  TO WF20-ZZ.
           MOVE WF19-TRANSACAO         TO WF20-TRANSACAO.
           MOVE WF19-FUNCAO            TO WF20-FUNCAO.
           MOVE WF19-FUNC-BDSCO        TO WF20-FUNC-BDSCO
           MOVE WF19-CAG-BCRIA         TO WF20-CAG-BCRIA.
           MOVE WF19-CCTA-CLI          TO WF20-CCTA-CLI.
           MOVE WF19-DANO-MES-SDO      TO WF20-DANO-MES-SDO.
           MOVE 'S'                    TO WF20-FIM.
           MOVE '0000'                 TO GFCTG2-COD-MSG.
           MOVE ZEROS                  TO WF20-COD-SQL-ERRO
                                          WF20-COD-MSG-ERRO
                                          WF20-ERRO.

           PERFORM 1110-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WF20-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM A DESCRISCAO DAS MENSAGENS DE ERRO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WF19-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WF19-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WF19-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0350'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WF20-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WF20-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0350'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WF20-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WF20-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WF20-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WF20-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONCATENAR MENSAGEM                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG(01:68) WRK-NOME-TAB

           DELIMITED BY '  '           INTO WF20-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA A SITUACAO DO SISTEMA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WF19-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WF19-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WF19-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WF20-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0350'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WRK-MSG-AUX
                   MOVE WRK-MSG-75     TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WF20-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0350'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO        TO WF20-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0350'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WF20-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE DADOS RECEBIDOS.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTENCIA-DADOS         SECTION.
      *----------------------------------------------------------------*

           IF (WF19-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (WF19-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (WF19-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (WF19-CAG-BCRIA        NOT NUMERIC) OR
              (WF19-CAG-BCRIA        EQUAL ZEROS) OR
              (WF19-CCTA-CLI         NOT NUMERIC) OR
              (WF19-CCTA-CLI         EQUAL ZEROS) OR
              (WF19-DANO-MES-SDO     NOT NUMERIC)
               MOVE 1                  TO WF20-ERRO
                                        WF20-COD-MSG-ERRO
               MOVE ZEROS              TO WF20-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WF20-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2050-DADOS-CADASTRAIS.

           PERFORM 2100-MONTAR-COMBO.

           IF WF19-DANO-MES-SDO GREATER THAN ZEROS
              PERFORM 2200-OBTER-DADOS
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    DADOS CADASTRAIS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2050-DADOS-CADASTRAIS           SECTION.
      *----------------------------------------------------------------*

           MOVE WF19-CAG-BCRIA             TO WRK-AGENCIA.
           MOVE WF19-CCTA-CLI              TO WRK-CONTA.

           MOVE 'CLIB1000'                 TO WRK-MODULO.

           CALL WRK-MODULO USING WRK-AREA-ENTRADA WRK-AREA-SAIDA.

           IF WRK-CODIGO-RETORNO NOT EQUAL ZEROS
              MOVE 'CLIENTE INEXISTENTE NO CLIE'
                                           TO WF20-NOME
              GO TO 2050-99-FIM
           END-IF.

           MOVE WRK-MENSAGEM-SAIDA(1:LENGTH OF WRK-AREA-RETORNO)
                                           TO WRK-AREA-RETORNO.

           IF TIPO-CONTA-C-P EQUAL 'C'
              MOVE NUM-CGCCPF              TO WRK-S9-9
              MOVE WRK-9-9                 TO WF20-NUM-CGCCPF
              MOVE FILIAL-CGCCPF           TO WRK-S9-9
              MOVE WRK-9-4                 TO WF20-FILIAL-CGCCPF
              MOVE CONTR-CGCCPF            TO WRK-X-2
              MOVE WRK-9-2                 TO WF20-CONTR-CGCCPF
              MOVE NOME                    TO WF20-NOME
           ELSE
              MOVE POUPADOR-CGC-CPF        TO WRK-S9-9
              MOVE WRK-9-9                 TO WF20-NUM-CGCCPF
              MOVE POUPADOR-FILIAL         TO WRK-S9-9
              MOVE WRK-9-4                 TO WF20-FILIAL-CGCCPF
              MOVE POUPADOR-CONTROLE       TO WRK-X-2
              MOVE WRK-9-2                 TO WF20-CONTR-CGCCPF
              MOVE POUPADOR-NOME-CLIE      TO WF20-NOME
           END-IF.

      *----------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTAR COMBO                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-MONTAR-COMBO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ABRIR-CURSOR

           PERFORM 2120-LER-CURSOR

           PERFORM VARYING IND-1 FROM 1 BY 1
             UNTIL WRK-FIM-GFCTB0P0  EQUAL 'S'
                OR IND-1 GREATER 3
                   MOVE DANO-MES-SDO OF GFCTB0P0
                     TO WF20-DANO-MES(IND-1)
                   PERFORM 2120-LER-CURSOR
           END-PERFORM.

           PERFORM 2140-FECHAR-CURSOR.

           COMPUTE IND-1 = IND-1 - 1.
           MOVE IND-1 TO WF20-QTDE-OCCUR.

           IF IND-1                EQUAL ZEROS
              MOVE ZEROS           TO WF20-COD-SQL-ERRO
              MOVE 'S'             TO WF20-FIM
              MOVE 1               TO WF20-ERRO
              MOVE 290             TO WF20-COD-MSG-ERRO
              MOVE '0290'          TO GFCTG2-COD-MSG
              PERFORM 1110-OBTER-DESC-MENSAGEM
              MOVE GFCTG3-DESC-MSG TO WF20-DESC-MSG-ERRO
              PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABERTURA DO CURSOR.                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO         OF GFCTB0P0.
           MOVE WF19-CAG-BCRIA         TO CAG-BCRIA    OF GFCTB0P0.
           MOVE WF19-CCTA-CLI          TO CCTA-CLI     OF GFCTB0P0.
           MOVE ZEROS                  TO DANO-MES-SDO OF GFCTB0P0.

           EXEC SQL OPEN CSR01-GFCTB0P0 END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0P0'      TO WRK-NOME-TAB
               MOVE 'GFCT0350'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPOSIC_SDO_APLIC '
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA(1:LENGTH OF GFCT0M-SQLCA-AREA)
                                       TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WF20-COD-SQL-ERRO
               MOVE 9                  TO WF20-ERRO
               MOVE 0005               TO WF20-COD-MSG-ERRO
               MOVE SPACES             TO WF20-DESC-MSG-ERRO
               PERFORM 1120-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LEITURA DO CURSOR.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL FETCH CSR01-GFCTB0P0
                     INTO :GFCTB0P0.DANO-MES-SDO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0P0'      TO WRK-NOME-TAB
               MOVE 'GFCT0350'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPOSIC_SDO_APLIC '
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE SQLCA(1:LENGTH OF GFCT0M-SQLCA-AREA)
                                       TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WF20-COD-SQL-ERRO
               MOVE 9                  TO WF20-ERRO
               MOVE 0006               TO WF20-COD-MSG-ERRO
               MOVE SPACES             TO WF20-DESC-MSG-ERRO
               PERFORM 1120-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL +100
               MOVE 'S'                TO WRK-FIM-GFCTB0P0
                                          WF20-FIM
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAMENTO DO CURSOR.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL CLOSE CSR01-GFCTB0P0 END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0011'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0P0'      TO WRK-NOME-TAB
               MOVE 'GFCT0350'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPOSIC_SDO_APLIC '
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE SQLCA(1:LENGTH OF GFCT0M-SQLCA-AREA)
                                       TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WF20-COD-SQL-ERRO
               MOVE 9                  TO WF20-ERRO
               MOVE 0011               TO WF20-COD-MSG-ERRO
               MOVE SPACES             TO WF20-DESC-MSG-ERRO
               PERFORM 1120-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DADOS
      *----------------------------------------------------------------*
       2200-OBTER-DADOS                SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CBCO         OF GFCTB0P0.
           MOVE WF19-CAG-BCRIA         TO CAG-BCRIA    OF GFCTB0P0.
           MOVE WF19-CCTA-CLI          TO CCTA-CLI     OF GFCTB0P0.
           MOVE WF19-DANO-MES-SDO      TO DANO-MES-SDO OF GFCTB0P0.

           EXEC SQL SELECT CSERVC_TARIF
                         , VSDO_MED_FUNDO
                         , VSDO_MED_POUP
                         , VSDO_MED_PREVD
                         , VSDO_MED_CERTF
                         , VSDO_APLIC_TSORO
                         , VSDO_APLIC_ACOES
                         , VSDO_APLIC_DBTRS
                         , VSDO_APLIC_DERIV
                      INTO :GFCTB0P0.CSERVC-TARIF
                         , :GFCTB0P0.VSDO-MED-FUNDO
                         , :GFCTB0P0.VSDO-MED-POUP
                         , :GFCTB0P0.VSDO-MED-PREVD
                         , :GFCTB0P0.VSDO-MED-CERTF
                         , :GFCTB0P0.VSDO-APLIC-TSORO
                         , :GFCTB0P0.VSDO-APLIC-ACOES
                         , :GFCTB0P0.VSDO-APLIC-DBTRS
                         , :GFCTB0P0.VSDO-APLIC-DERIV
                      FROM DB2PRD.TPOSIC_SDO_APLIC
                     WHERE CBCO         = :GFCTB0P0.CBCO
                       AND CAG_BCRIA    = :GFCTB0P0.CAG-BCRIA
                       AND CCTA_CLI     = :GFCTB0P0.CCTA-CLI
                       AND DANO_MES_SDO = :GFCTB0P0.DANO-MES-SDO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0P0'      TO WRK-NOME-TAB
               MOVE 'GFCT0350'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPOSIC_SDO_APLIC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA(1:LENGTH OF GFCT0M-SQLCA-AREA)
                                       TO GFCT0M-SQLCA-AREA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WF20-COD-SQL-ERRO
               MOVE 9                  TO WF20-ERRO
               MOVE 0006               TO WF20-COD-MSG-ERRO
               MOVE SPACES             TO WF20-DESC-MSG-ERRO
               PERFORM 1120-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CSERVC-TARIF     OF GFCTB0P0 TO WF20-CSERVC-TARIF.
           MOVE VSDO-MED-FUNDO   OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-MED-FUNDO.
           MOVE VSDO-MED-POUP    OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-MED-POUP.
           MOVE VSDO-MED-PREVD   OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-MED-PREVD.
           MOVE VSDO-MED-CERTF   OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-MED-CERTF.
           MOVE VSDO-APLIC-TSORO OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-APLIC-TSORO.
           MOVE VSDO-APLIC-ACOES OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-APLIC-ACOES.
           MOVE VSDO-APLIC-DBTRS OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-APLIC-DBTRS.
           MOVE VSDO-APLIC-DERIV OF GFCTB0P0 TO WRK-VSDO-CS.
           MOVE WRK-VSDO-SS                  TO WF20-VSDO-APLIC-DERIV.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FINALIZAR O PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WF20-ERRO        NOT EQUAL ZEROS
               MOVE WF19-FUNC-BDSCO    TO WF20-FUNC-BDSCO
               MOVE WF19-CAG-BCRIA     TO WF20-CAG-BCRIA
               MOVE WF19-CCTA-CLI      TO WF20-CCTA-CLI
               MOVE WF19-DANO-MES-SDO  TO WF20-DANO-MES-SDO
           END-IF.

           PERFORM 3100-RETORNAR-CHAMADOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA RETORNAR AO CHAMADOR.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

