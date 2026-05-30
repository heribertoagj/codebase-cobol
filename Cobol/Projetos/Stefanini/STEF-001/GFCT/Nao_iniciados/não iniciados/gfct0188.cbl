      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0188.
       AUTHOR.     JEAN SANTOS ALVES.
      *================================================================*
      *               C P M  B R A X I S  S I S T E M A S              *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0188                                     *
      *    PROGRAMADOR  : JEAN SANTOS ALVES        - CPM BRAXIS/FPOLIS *
      *    ANALISTA CPM : MAURICIO NAPOLEAO        - CPM BRAXIS/FPOLIS *
      *    ANALISTA     : HELIO SANTONI - PROCWORK - GRUPO 50          *
      *    DATA         : 14/07/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA DE SOLICITACAO DE CONTAS DO AGRUPAMENTO          *
      *      PERSONALIZADO.                                            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TPO_AGPTO_CTA                   GFCTB0F3          *
      *       DB2PRD.TCTA_AGPTO_PERSO                GFCTB0J5          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      GFCT5522 - VERIFICA DISPONIBILIDADE ON LINE.              *
      *      GFCT5006 - OBTER DESCRICAO DA AGENCIA.                    *
      *      GFCT5054 - OBTER DADOS DO CLIENTE.                        *
      *      SARH8000 - OBTER NOME DO FUNCIONARIO.                     *
      *                                                                *
BI1211*----------------------------------------------------------------*
BI1211*    ULTIMA ALTERACAO EM 17/01/2012 - BIRA                       *
BI1211*    - MANTER RETURN-CODE = 0 APOS RETORNO DO MODULO SARH8000.   *
BI1211*                                                                *
      *================================================================*
RU1118*    ULTIMA ALTERACAO EM 14/11/2018 - MARCIO A. RUI              *
RU1118*    - INCLUSAO DE TRATAMENTO PARA CODIDO MATRICULA FUNCIONARIO  *
RU1118*      IGUAL A 1857 - PROGRAMA DE CANCELAMENTO MASSIVO           *
RU1118*                                                                *
      *================================================================*
RU0725*    ULTIMA ALTERACAO EM 22/07/2025 - MARCIO A. RUI              *
RU0725*    - INCLUSAO DE TRATAMENTO PARA CODIDO MATRICULA FUNCIONARIO  *
RU0725*      IGUAL A 1859 - ENCERRAMENTO RETENCAO                      *
RU0725*                                                                *
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
           '*  INICIO DA WORKING GFCT0188  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-DESC-MSG-R.
               10  WRK-DESC-MENSAGEM   PIC X(70)           VALUE SPACES.
               10  FILLER              PIC X(09)           VALUE SPACES.
           05  WRK-DESC-MSG            REDEFINES
               WRK-DESC-MSG-R          PIC X(79).
           05  WRK-SQLCODE             PIC +9(09)          VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-SQLCODE.
               10  FILLER              PIC  9(07).
               10  WRK-SQLCODE-R       PIC  9(03).
           05  WRK-CFUNC-BDSCO         PIC +9(09)          VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-CFUNC-BDSCO.
               10  FILLER              PIC X(03).
               10  WRK-FUNC-BDSCO      PIC X(07).
           05  WRK-S9-9                PIC +9(09)          VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-S9-9.
               10  FILLER              PIC  X(01).
               10  WRK-9-9             PIC  9(09).
           05  WRK-COD-TABELA          PIC  X(09)          VALUE SPACES.
           05  WRK-9-13                PIC 9(13)           VALUE ZEROS.
           05  FILLER                  REDEFINES WRK-9-13.
             10  FILLER                PIC 9(06).
             10  WRK-9-07              PIC 9(07).
           05  WRK-GFCTJE-NOME-FUNC    PIC X(040)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE CAMPOS NULOS  *'.
      *----------------------------------------------------------------*

       01  WRK-GFCTB0J5-NULL.
           05 WRK-CFUNC-INCL-SIST-NULL PIC S9(04) COMP     VALUE ZEROS.
           05 WRK-CFUNC-ENCRR-VGCIA-NULL
                                       PIC S9(04) COMP     VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG01.
               10  FILLER               PIC X(26)          VALUE
                   'ERRO NA CHAMADA DO MODULO '.
               10  WRK-NOME-MOD         PIC X(08)          VALUE SPACES.
               10  FILLER               PIC X(41)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA SARH8000       *'.
      *----------------------------------------------------------------*

       01  WRK-SARH8000-ENT.
           05  WRK-SARH8000-PESQ-ENT   PIC 9(09)           VALUE ZEROS.
           05  WRK-SARH8000-TAM-ENT    PIC 9(04)           VALUE ZEROS.
           05  WRK-SARH8000-SOLIC-ENT.
             10  FILLER                OCCURS 255 TIMES.
               15  WRK-SARH8000-CAMPO-ENT
                                       PIC 9(03)           VALUE ZEROS.

       01  WRK-SARH8000-SAI.
           05  WRK-SARH8000-COD-RET-SAI
                                       PIC 9(02)           VALUE ZEROS.
           05  WRK-SARH8000-TAM-SAI    PIC 9(04)           VALUE ZEROS.
           05  WRK-SARH8000-REG-SAI.
             10  FILLER                OCCURS 0 TO 1000 TIMES
                 DEPENDING ON          WRK-SARH8000-TAM-SAI.
               15  WRK-SARH8000-POSI-SAI
                                       PIC X(01)           VALUE SPACES.

       01  WRK-SARH8000-AREA-RET.
           05  WRK-SARH8000-NOME-RET   PIC X(80)           VALUE SPACES.

       01  WRK-ERRO-AREA               PIC X(107)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.
       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5006   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.
       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
            '*    AREA DO MODULO GFCT5054   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG0'.
       COPY 'I#GFCTHA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0J5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*    FIM DA WORKING GFCT0188   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTJD'.
       COPY 'I#GFCTJE'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTJD-ENTRADA
                                             GFCTJE-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA PRINCIPAL                                               *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR.

           PERFORM 0700-PROCESSAR.

           PERFORM 0400-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONTROLE GERAL DE PROCESSOS                                    *
      *----------------------------------------------------------------*
       0100-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA
                                          GFCTG3-SAIDA
                                          GFCTJE-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTFZ-ENTRADA
                                          GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCTG0-ENTRADA
                                          GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTG1-SAIDA
                      GFCTG3-SAIDA
                      GFCTJE-SAIDA
                      GFCTG2-ENTRADA
                      GFCTFZ-ENTRADA
                      GFCT1V-ENTRADA
                      GFCT1X-SAIDA
                      GFCTG0-ENTRADA
                      GFCTHA-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE ZEROS                  TO GFCT0M-SQLCODE.

           PERFORM 0200-INICIALIZAR-SAIDA.

           PERFORM 0500-TRATAR-DISP-SISTEMA.

           PERFORM 0600-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA INICIALIZAR AREA DE SAIDA                          *
      *----------------------------------------------------------------*
       0200-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF GFCTJE-SAIDA TO GFCTJE-LL.
           MOVE ZEROS                  TO GFCTJE-ZZ.
           MOVE GFCTJD-TRANSACAO       TO GFCTJE-TRANSACAO.
           MOVE GFCTJD-FUNCAO          TO GFCTJE-FUNCAO.

           IF  GFCTJD-QTDE-OCOR        IS NUMERIC
               MOVE GFCTJD-QTDE-OCOR   TO GFCTJE-QTDE-OCOR
           END-IF.

           IF  GFCTJD-QTDE-TOT-REG     IS NUMERIC
               MOVE GFCTJD-QTDE-TOT-REG
                                       TO GFCTJE-QTDE-TOT-REG
           END-IF.

           MOVE GFCTJD-FUNC-BDSCO      TO GFCTJE-FUNC-BDSCO.
           MOVE 'S'                    TO GFCTJE-FIM.
           MOVE ZEROS                  TO GFCTJE-COD-SQL-ERRO
                                          GFCTJE-COD-MSG-ERRO
                                          GFCTJE-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 0300-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO WRK-DESC-MENSAGEM.
           MOVE WRK-DESC-MSG           TO GFCTJE-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM                        *
      *----------------------------------------------------------------*
       0300-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE LENGTH OF GFCTG2-ENTRADA
                                       TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE GFCTJD-TRANSACAO       TO GFCTG2-TRANSACAO.
           MOVE GFCTJD-FUNCAO          TO GFCTG2-FUNCAO.
           MOVE GFCTJD-FUNC-BDSCO      TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO GFCTJE-DESC-MSG-ERRO
               MOVE 9                  TO GFCTJE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO GFCTJE-DESC-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTJE-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTJE-DESC-MSG-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO GFCTJE-ERRO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO GFCTJE-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO GFCTJE-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO GFCTJE-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO GFCTJE-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA RETORNAR AO CHAMADOR                               *
      *----------------------------------------------------------------*
       0400-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA VERIFICAR DISPONIBILIDADE DO SISTEMA               *
      *----------------------------------------------------------------*
       0500-TRATAR-DISP-SISTEMA        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCTJD-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCTJD-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE GFCTJD-FUNC-BDSCO      TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO GFCTJE-DESC-MSG-ERRO
               MOVE 9                  TO GFCTJE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO GFCTJE-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCTJE-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO GFCTJE-DESC-MSG-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO GFCTJE-ERRO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO GFCTJE-ERRO
               MOVE SPACES             TO GFCTJE-DESC-MSG-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CONSISTIR CAMPOS DE ENTRADA                        *
      *----------------------------------------------------------------*
       0600-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTJD-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJD-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJD-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
              (GFCTJD-AGRUPAMENTO      NOT NUMERIC)                OR
              (GFCTJD-AGRUPAMENTO      EQUAL ZEROS)                OR
              (GFCTJD-AGENCIA          NOT NUMERIC)                OR
              (GFCTJD-AGENCIA          EQUAL ZEROS)                OR
              (GFCTJD-CONTA            NOT NUMERIC)                OR
              (GFCTJD-CONTA            EQUAL ZEROS)                OR
              (GFCTJD-DATA-VIGENCIA    EQUAL SPACES)               OR
              (GFCTJD-QTDE-OCOR        NOT NUMERIC)                OR
              (GFCTJD-QTDE-TOT-REG     NOT NUMERIC)                OR
              (GFCTJD-TIPO-CONS        NOT EQUAL 'C' AND 'H')
               MOVE 1                  TO GFCTJE-ERRO
                                          GFCTJE-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTJE-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 0300-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WRK-DESC-MENSAGEM
               MOVE WRK-DESC-MSG       TO GFCTJE-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       0700-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0800-ACESSAR-GFCTBF3.

           PERFORM 0900-ACESSAR-GFCT5006.

           PERFORM 1000-ACESSAR-GFCT5054.

           PERFORM 1100-ACESSAR-GFCTBJ5.

           MOVE GFCTJD-AGRUPAMENTO     TO GFCTJE-AGRUPAMENTO.
           MOVE GFCTJD-AGENCIA         TO GFCTJE-AGENCIA.
           MOVE GFCTJD-CONTA           TO GFCTJE-CONTA.
           MOVE GFCTJD-TIPO-CONS       TO GFCTJE-TIPO-CONS.
           MOVE DINIC-VGCIA-REG OF GFCTB0J5
                                       TO GFCTJE-DATA-INIC-VIG.

           MOVE DFIM-VGCIA-REG OF GFCTB0J5
                                       TO GFCTJE-DATA-FIM-VIG.

           MOVE CFUNC-INCL-SIST OF GFCTB0J5
                                       TO WRK-CFUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO         TO GFCTJE-FUNC-INCL.
           MOVE HINCL-REG-SIST OF GFCTB0J5(09:02)
                                       TO GFCTJE-DATA-INCL(01:02).
           MOVE '.'                    TO GFCTJE-DATA-INCL(03:01).
           MOVE HINCL-REG-SIST OF GFCTB0J5(06:02)
                                       TO GFCTJE-DATA-INCL(04:02).
           MOVE '.'                    TO GFCTJE-DATA-INCL(06:01).
           MOVE HINCL-REG-SIST OF GFCTB0J5(01:04)
                                       TO GFCTJE-DATA-INCL(07:04).
           MOVE HINCL-REG-SIST OF GFCTB0J5(12:02)
                                       TO GFCTJE-HORA-INCL(01:02).
           MOVE '.'                    TO GFCTJE-HORA-INCL(03:01).
           MOVE HINCL-REG-SIST OF GFCTB0J5(15:02)
                                       TO GFCTJE-HORA-INCL(04:02).
           MOVE CFUNC-ENCRR-VGCIA OF GFCTB0J5
                                       TO WRK-CFUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO         TO GFCTJE-FUNC-EXCL.
           MOVE HVGCIA-REGRA OF GFCTB0J5(09:02)
                                       TO GFCTJE-DATA-EXCL(01:02).
           MOVE '.'                    TO GFCTJE-DATA-EXCL(03:02).
           MOVE HVGCIA-REGRA OF GFCTB0J5(06:02)
                                       TO GFCTJE-DATA-EXCL(04:02).
           MOVE '.'                    TO GFCTJE-DATA-EXCL(06:02).
           MOVE HVGCIA-REGRA OF GFCTB0J5(01:04)
                                       TO GFCTJE-DATA-EXCL(07:04).
           MOVE HVGCIA-REGRA OF GFCTB0J5(12:02)
                                       TO GFCTJE-HORA-EXCL(01:02).
           MOVE '.'                    TO GFCTJE-HORA-EXCL(03:02).
           MOVE HVGCIA-REGRA OF GFCTB0J5(15:02)
                                       TO GFCTJE-HORA-EXCL(04:02).
           MOVE CIDTFD-PROCM-INCL OF GFCTB0J5
                                       TO GFCTJE-CANAL-INCL.
           MOVE CIDTFD-PROCM-ENCRR OF GFCTB0J5
                                       TO GFCTJE-CANAL-EXCL.

           IF CFUNC-INCL-SIST OF GFCTB0J5
                                       EQUAL 1854
               MOVE 'VIA PROGRAMA GFCT1854'
                                       TO GFCTJE-NOME-FUNC-INCL
           ELSE
RU0725       IF CFUNC-INCL-SIST OF GFCTB0J5
RU0725                                  EQUAL 1859
RU0725          MOVE 'ENCERRAMENTO DE CONTA - RETENCAO'
RU0725                                 TO GFCTJE-NOME-FUNC-INCL
             ELSE
               IF CFUNC-INCL-SIST OF GFCTB0J5
                                       EQUAL ZEROS
                   MOVE SPACES             TO GFCTJE-NOME-FUNC-INCL
               ELSE
                   MOVE CFUNC-INCL-SIST OF GFCTB0J5
                                       TO WRK-S9-9
                   MOVE WRK-9-9            TO WRK-SARH8000-PESQ-ENT
                   PERFORM 1200-OBTER-NOME-FUNC
BI0112             MOVE ZEROS              TO RETURN-CODE
                   MOVE WRK-GFCTJE-NOME-FUNC
                                       TO GFCTJE-NOME-FUNC-INCL
               END-IF
             END-IF
           END-IF.

RU1118     IF CFUNC-ENCRR-VGCIA OF GFCTB0J5
RU1118                                 EQUAL 1857
RU1118         MOVE 'VIA PROGRAMA GFCT1857'
RU1118                                 TO GFCTJE-NOME-FUNC-EXCL
RU1118     ELSE
              IF CFUNC-ENCRR-VGCIA OF GFCTB0J5
                                       EQUAL ZEROS
                 MOVE SPACES             TO GFCTJE-NOME-FUNC-EXCL
              ELSE
                 MOVE CFUNC-ENCRR-VGCIA OF GFCTB0J5
                                         TO WRK-S9-9
                 MOVE WRK-9-9            TO WRK-SARH8000-PESQ-ENT
                 PERFORM 1200-OBTER-NOME-FUNC
                 MOVE WRK-GFCTJE-NOME-FUNC
                                         TO GFCTJE-NOME-FUNC-EXCL
              END-IF
RU1118     END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA OBTER DADOS DA TABELA GFCTB0F3                     *
      *----------------------------------------------------------------*
       0800-ACESSAR-GFCTBF3            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTJD-AGRUPAMENTO     TO CAGPTO-CTA OF GFCTB0F3.

           EXEC SQL
             SELECT RAGPTO_CTA
             INTO   :GFCTB0F3.RAGPTO-CTA
             FROM   DB2PRD.TPO_AGPTO_CTA
             WHERE CAGPTO_CTA          = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 0300-OBTER-DESC-MENSAGEM
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '-GFCTB0F3'        TO WRK-COD-TABELA
               MOVE 'GFCT0188'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TPPO_AGPTO_CTA'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTJE-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-SQLCODE-R     TO GFCTJE-COD-SQL-ERRO
               MOVE SPACES             TO GFCTJE-DESC-MSG-ERRO
               MOVE  0010              TO GFCTJE-COD-MSG-ERRO
               STRING GFCTG3-DESC-MSG
                      WRK-COD-TABELA
               DELIMITED BY SIZE INTO GFCTJE-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR
           END-IF.

           MOVE RAGPTO-CTA OF GFCTB0F3 TO GFCTJE-DESCR-AGRUP.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ACESSAR O MODULO GFCT5006.                         *
      *----------------------------------------------------------------*
       0900-ACESSAR-GFCT5006           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA.
           MOVE LENGTH OF GFCT1V-ENTRADA
                                       TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTJD-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTJD-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTJD-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE GFCTJD-AGENCIA         TO GFCT1V-COD-DEPDC(1).
           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                  TO GFCTJE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF GFCT1X-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTJE-FIM
               MOVE GFCT1X-ERRO        TO GFCTJE-ERRO
               MOVE GFCT1X-COD-SQL-ERRO
                                       TO GFCTJE-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO
                                       TO GFCTJE-COD-MSG-ERRO
               IF GFCT1X-ERRO          EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTJE-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'
                           GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTJE-DESC-MSG-ERRO
               END-IF
               PERFORM 0400-FINALIZAR
           END-IF.

           MOVE GFCT1X-DESC-DEPDC(1)(1:40)
                                       TO GFCTJE-DESCR-AG.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ACESSAR O MODULO GFCT5054.                         *
      *----------------------------------------------------------------*
       1000-ACESSAR-GFCT5054           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG0-ENTRADA.
           MOVE LENGTH OF GFCTG0-ENTRADA
                                       TO GFCTG0-LL.
           MOVE ZEROS                  TO GFCTG0-ZZ.
           MOVE GFCTJD-TRANSACAO       TO GFCTG0-TRANSACAO.
           MOVE GFCTJD-FUNCAO          TO GFCTG0-FUNCAO.
           MOVE GFCTJD-FUNC-BDSCO      TO GFCTG0-FUNC-BDSCO.
           MOVE GFCTJD-AGENCIA         TO GFCTG0-COD-DEPDC.
           MOVE GFCTJD-CONTA           TO WRK-9-13.
           MOVE WRK-9-07               TO GFCTG0-CCTA-CLI.
           MOVE 'N'                    TO GFCTG0-FIM.
           MOVE SPACES                 TO GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTHA-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 'GFCT5054'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG0-ENTRADA
                                             GFCTHA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF RETURN-CODE              NOT EQUAL ZEROS
               MOVE 9                  TO GFCTJE-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0188'         TO GFCT0M-TRANSACAO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF GFCTHA-ERRO              NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTJE-FIM
               MOVE GFCTHA-ERRO        TO GFCTJE-ERRO
               MOVE GFCTHA-COD-SQL-ERRO
                                       TO GFCTJE-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO
                                       TO GFCTJE-COD-MSG-ERRO
               IF GFCTHA-ERRO          EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO
                                       TO GFCTJE-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5054-'
                           GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTJE-DESC-MSG-ERRO
               END-IF
               PERFORM 0400-FINALIZAR
           END-IF.

           MOVE GFCTHA-NOME-CLIENTE(1:40)
                                       TO GFCTJE-NOME.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ACESSAR O MODULO GFCTBJ5.                          *
      *----------------------------------------------------------------*
       1100-ACESSAR-GFCTBJ5            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTJD-AGRUPAMENTO     TO CAGPTO-CTA OF GFCTB0J5.
           MOVE 237                    TO CBCO OF GFCTB0J5.
           MOVE GFCTJD-AGENCIA         TO CAG-BCRIA OF GFCTB0J5.
           MOVE GFCTJD-CONTA           TO CCTA-BCRIA-CLI OF GFCTB0J5.
           MOVE GFCTJD-DATA-VIGENCIA   TO DINIC-VGCIA-REG OF GFCTB0J5.

           EXEC SQL
             SELECT CAGPTO_CTA,
                    CAG_BCRIA,
                    CCTA_BCRIA_CLI,
                    DINIC_VGCIA_REG,
                    DFIM_VGCIA_REG,
                    CIDTFD_PROCM_INCL,
                    CFUNC_INCL_SIST,
                    HINCL_REG_SIST,
                    CIDTFD_PROCM_ENCRR,
                    CFUNC_ENCRR_VGCIA,
                    HVGCIA_REGRA
             INTO  :GFCTB0J5.CAGPTO-CTA,
                   :GFCTB0J5.CAG-BCRIA,
                   :GFCTB0J5.CCTA-BCRIA-CLI,
                   :GFCTB0J5.DINIC-VGCIA-REG,
                   :GFCTB0J5.DFIM-VGCIA-REG,
                   :GFCTB0J5.CIDTFD-PROCM-INCL,
                   :GFCTB0J5.CFUNC-INCL-SIST
                     :WRK-CFUNC-INCL-SIST-NULL,
                   :GFCTB0J5.HINCL-REG-SIST,
                   :GFCTB0J5.CIDTFD-PROCM-ENCRR,
                   :GFCTB0J5.CFUNC-ENCRR-VGCIA
                     :WRK-CFUNC-ENCRR-VGCIA-NULL,
                   :GFCTB0J5.HVGCIA-REGRA
             FROM   DB2PRD.TCTA_AGPTO_PERSO
             WHERE CAGPTO_CTA         = :GFCTB0J5.CAGPTO-CTA         AND
                   CBCO               = :GFCTB0J5.CBCO               AND
                   CAG_BCRIA          = :GFCTB0J5.CAG-BCRIA          AND
                   CCTA_BCRIA_CLI     = :GFCTB0J5.CCTA-BCRIA-CLI     AND
                   DINIC_VGCIA_REG    = :GFCTB0J5.DINIC-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 0300-OBTER-DESC-MENSAGEM
               INITIALIZE                 GFCT0M-ERRO-SQL
               MOVE '-GFCTB0J5'        TO WRK-COD-TABELA
               MOVE 'GFCT0188'         TO GFCT0M-PROGRAMA
                                          GFCT0M-TRANSACAO
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT '          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTJE-ERRO
               MOVE  SQLCODE           TO WRK-SQLCODE
               MOVE  WRK-SQLCODE-R     TO GFCTJE-COD-SQL-ERRO
               MOVE SPACES             TO GFCTJE-DESC-MSG-ERRO
               MOVE  0010              TO GFCTJE-COD-MSG-ERRO
               STRING GFCTG3-DESC-MSG
                      WRK-COD-TABELA
               DELIMITED BY SIZE INTO GFCTJE-DESC-MSG-ERRO
               PERFORM 0400-FINALIZAR
           END-IF.

           IF  WRK-CFUNC-INCL-SIST-NULL
                                       LESS ZEROS
               MOVE ZEROS              TO CFUNC-INCL-SIST   OF GFCTB0J5
           END-IF.

           IF  WRK-CFUNC-ENCRR-VGCIA-NULL
                                       LESS ZEROS
               MOVE ZEROS              TO CFUNC-ENCRR-VGCIA OF GFCTB0J5
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA ACESSAR O MODULO SARH8000.                         *
      *----------------------------------------------------------------*
       1200-OBTER-NOME-FUNC            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SARH8000-REG-SAI.
           MOVE 86                     TO WRK-SARH8000-TAM-ENT.
           MOVE 002                    TO WRK-SARH8000-CAMPO-ENT(1).
           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-SARH8000-ENT
                                             WRK-SARH8000-SAI
                                             WRK-ERRO-AREA
                                             SQLCA.

           EVALUATE WRK-SARH8000-COD-RET-SAI

               WHEN ZEROS
                   MOVE WRK-SARH8000-REG-SAI(1:80)
                                       TO WRK-SARH8000-AREA-RET
                   MOVE WRK-SARH8000-NOME-RET(1:40)
                                       TO WRK-GFCTJE-NOME-FUNC

               WHEN 10
               WHEN 12
                   MOVE ALL '*'         TO WRK-GFCTJE-NOME-FUNC

               WHEN OTHER
                   MOVE SPACES          TO WRK-GFCTJE-NOME-FUNC

           END-EVALUATE.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

