      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5050.
       AUTHOR.     MARCUS VINICIUS.
      *================================================================*
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5050                                    *
      *    ANALISTA....:   MARCUS VINICIUS         - PWI / GRUPO 50    *
      *    DATA........:   09/2007                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   OBTEM JUNCAO E INFORMA SE AGENCIA S/N.      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA DO MODULO GFCT5523 *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA DO MODULO GFCT5523   *
      *    I#GFCTAI - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTAJ - AREA DE COMUNICACAO - SAIDA.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    SENH0315 - OBTER JUNCAO.                                    *
      *================================================================*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
RU0720*----------------------------------------------------------------*
RU0720*                                                                *
RU0720*    PROGRAMADOR  : MARCIO ALEXANDRO RUI                         *
RU0720*    ANALISTA     : MARCIO ALEXANDRO RUI                         *
RU0720*    DATA         : 07/2020 - 08/2020                            *
RU0720*                                                                *
RU0720*    OBJETIVO     :                                              *
RU0720*    - 2020-0617520-5-003                                        *
RU0720*      LIBERAR O PREENCHIMENTO AUTOMATICO DO CAMPO AGENCIA       *
RU0720*                                                                *
RU0720*================================================================*
           EJECT
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
           '*** GFCT5050 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-NOME-TABELA.
           03  FILLER                  PIC  X(003)         VALUE ' - '.
           03  WRK-NOME-TAB            PIC  X(008)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

           EJECT

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO SENH0315 ***'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
RU0720 01  FILLER REDEFINES WRK-COD-JUNCAO-0315.
RU0720     03  WRK-COD-JUNCAO-0315X    PIC  X(005).
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.

       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
HEXA       COPY I#BRAD7C.
HEXA  *


           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT5050 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTAI.

           EJECT
       COPY I#GFCTAJ.

           EJECT
       COPY I#GFCT0M.

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCTAI-ENTRADA
                                             GFCTAJ-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTAJ-SAIDA
                                          WRK-MENSAGEM-0315
                                          WRK-NOME-FUNC-0315
                                          WRK-NOME-BANCO-0315
                                          WRK-NOME-DEPTO-0315
                                          WRK-STATUS-0315.

           INITIALIZE                  GFCT0M-AREA-ERROS
                                       GFCTAJ-SAIDA
                                       WRK-MENSAGEM-0315
                                       WRK-COD-BANCO-0315
                                       WRK-CNPJ-BANCO-0315
                                       WRK-COD-JUNCAO-0315
                                       WRK-SECAO-0315.

           PERFORM 110000-INICIALIZA-SAIDA.

           PERFORM 120000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       110000-INICIALIZA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE 200                    TO GFCTAJ-LL.
           MOVE GFCTAI-ZZ              TO GFCTAJ-ZZ.
           MOVE GFCTAI-TRANSACAO       TO GFCTAJ-TRANSACAO.
           MOVE GFCTAI-FUNCAO          TO GFCTAJ-FUNCAO.

           IF  GFCTAI-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTAJ-QTDE-OCOR
           ELSE
               MOVE GFCTAI-QTDE-OCOR   TO GFCTAJ-QTDE-OCOR
           END-IF.

           IF  GFCTAI-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTAJ-QTDE-TOT-REG
           ELSE
               MOVE GFCTAI-QTDE-TOT-REG TO GFCTAJ-QTDE-TOT-REG
           END-IF.

           MOVE GFCTAI-FUNC-BDSCO      TO GFCTAJ-FUNC-BDSCO.
           MOVE GFCTAI-COD-FUNC        TO GFCTAJ-COD-FUNC.

           MOVE 'N'                    TO GFCTAJ-FIM.
           MOVE ZEROS                  TO GFCTAJ-ERRO
                                          GFCTAJ-COD-SQL-ERRO
                                          GFCTAJ-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 111000-OBTER-DESCRICAO-MSG.

           MOVE GFCTG3-DESC-MSG        TO GFCTAJ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTAI-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTAI-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTAI-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTAJ-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5050'         TO GFCT0M-TRANSACAO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO GFCTAJ-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5050'         TO GFCT0M-TRANSACAO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO GFCTAJ-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTAJ-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTAJ-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTAJ-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF (GFCTAI-TRANSACAO        EQUAL SPACES OR LOW-VALUES OR
               GFCTAI-FUNCAO           EQUAL SPACES OR LOW-VALUES OR
               GFCTAI-QTDE-OCOR        NOT NUMERIC  OR
               GFCTAI-QTDE-TOT-REG     NOT NUMERIC  OR
               GFCTAI-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES OR
               GFCTAI-COD-FUNC         NOT NUMERIC  OR
               GFCTAI-FIM              NOT EQUAL 'S' AND 'N')
               MOVE 1                  TO GFCTAJ-ERRO
                                          GFCTAJ-COD-MSG-ERRO
               MOVE 0001               TO GFCTG2-COD-MSG
               MOVE ZEROS              TO GFCTAJ-COD-SQL-ERRO
               MOVE 'S'                TO GFCTAJ-FIM
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG    TO GFCTAJ-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-OBTER-COD-JUNCAO.

RU0720     MOVE  WRK-COD-JUNCAO-0315X TO  GFCTAJ-JUNCAO

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  * HX_IF  WRK-COD-JUNCAO-0315  LESS  4000
HEXA  *
HEXA       MOVE        WRK-COD-JUNCAO-0315 TO  WRK-MESUX0-AGENCIA
HEXA       PERFORM     9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA       IF  WRK-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
               MOVE  'S'  TO  GFCTAJ-SE-AGENCIA
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

           EJECT
      *----------------------------------------------------------------*
       210000-OBTER-COD-JUNCAO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTAI-COD-FUNC             TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'                  USING WRK-VERSAO-0315
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

           IF  RETURN-CODE                    NOT EQUAL ZEROS AND 4
               MOVE 1                         TO GFCTAJ-ERRO
               MOVE RETURN-CODE               TO GFCTAJ-COD-SQL-ERRO
               MOVE 0056                      TO GFCTAJ-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE GFCTG3-DESC-MSG           TO GFCTAJ-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  RETURN-CODE                    EQUAL 4
               EVALUATE  WRK-COD-MENSAGEM-0315

                   WHEN 001
                       MOVE 1                 TO GFCTAJ-ERRO
                       MOVE ZEROS             TO GFCTAJ-COD-SQL-ERRO
                       MOVE 0057              TO GFCTAJ-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTAJ-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 154
                       MOVE 1                 TO GFCTAJ-ERRO
                       MOVE ZEROS             TO GFCTAJ-COD-SQL-ERRO
                       MOVE 0058              TO GFCTAJ-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTAJ-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 242
                       MOVE 1                 TO GFCTAJ-ERRO
                       MOVE ZEROS             TO GFCTAJ-COD-SQL-ERRO
                       MOVE 0059              TO GFCTAJ-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTAJ-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 009
                       MOVE ZEROS             TO GFCTAJ-ERRO
                                                 GFCTAJ-COD-SQL-ERRO
                                                 GFCTAJ-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTAJ-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR

                   WHEN 138
                       MOVE 1                 TO GFCTAJ-ERRO
                       MOVE ZEROS             TO GFCTAJ-COD-SQL-ERRO
                       MOVE 0060              TO GFCTAJ-COD-MSG-ERRO
                                                 GFCTG2-COD-MSG
                       PERFORM 111000-OBTER-DESCRICAO-MSG
                       MOVE GFCTG3-DESC-MSG   TO GFCTAJ-DESC-MSG-ERRO
                       PERFORM 300000-FINALIZAR
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

           EJECT
      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
HEXA  *
HEXA       IF          WRK-EH-ERRO-MESU-ABEND
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA'
HEXA             TO    ERR-TEXTO
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND
HEXA       ELSE
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA
HEXA       END-IF.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       MOVE    'GFCT5050'      TO          ERR-PGM.
HEXA  *
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0
HEXA                                           ERRO-AREA.
HEXA  *
HEXA       GOBACK.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
