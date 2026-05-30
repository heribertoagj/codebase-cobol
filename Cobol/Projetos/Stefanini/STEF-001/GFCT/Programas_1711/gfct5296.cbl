      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT5296.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT5296                                    *
      *    PROGRAMADOR.:   HELIO SANTONI             - PROCWORK GP. 50 *
      *    ANALISTA....:   WELLEN FRANCA             - PROCWORK GP. 50 *
      *    DATA........:   24/03/2010                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   VERIFICA SE AGENCIA EXISTE COMO             *
      *                    BANCO POSTAL OU BRADESCO EXPRESSO.          *
      *                                                                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.TCAD_CORSP_BCRIO       YMKCB001       *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    GFCTWAM2 - ENTRADA                                          *
      *    GFCTWAM3 - SAIDA                                            *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *    I#MESUM6 - AREA PARA O MODULO MESU9015.                     *
      *    I#MESUM8 - AREA PARA O MODULO MESU9018.                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    MESU9015 - CONSISTIR AGENCIA                                *
      *    MESU9018 - CONSISTIR PAB                                    *
      *    RURC9020 - VERIFICA MUNICIPIO                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
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
           '* AREA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(011)         VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

       01  WRK-DESC-POSTO-70           PIC  X(070)         VALUE SPACES.
       01  WRK-FILLER                  REDEFINES WRK-DESC-POSTO-70.
           05 WRK-DESC-POSTO-20        PIC  X(20).
           05 FILLER                   PIC  X(50).

       01  WRK-CPAB-9-05               PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                  REDEFINES WRK-CPAB-9-05.
           05 FILLER                   PIC  9(02).
           05 WRK-CPAB-9-03            PIC  9(03).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*   AREA PARA VARIAVEIS DE NULO *'.
      *----------------------------------------------------------------*

       01  WRK-YMKCB001-NULL.
           05  WRK-ICORSP-BCRIO-NULL   PIC S9(004) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM KCIM0184 - ENTRADA *'.
      *----------------------------------------------------------------*

       COPY KCIMW184.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 - SAIDA *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG1.

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
           '* AREA DE COMUNICACAO COM MESU9018 *'.
      *----------------------------------------------------------------*

       COPY I#MESUM8.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM MESU9015 *'.
      *----------------------------------------------------------------*

       COPY I#MESUM6.

       01  WRK-ERRO-AREA                   PIC  X(107)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAM2.

       COPY GFCTWAM3.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING WAM2-ENTRADA
                                                      WAM3-SAIDA
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
      * INICIALIZAR CAMPOS DE SAIDA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          WAM3-SAIDA.

           INITIALIZE WAM3-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA DISPONIBILIDADE DO SISTEMA                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAM2-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAM2-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAM2-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAM3-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
                                          WAM3-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5296'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO          EQUAL 9
               MOVE 9                  TO WAM3-ERRO
               MOVE 'GFCT5296'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          WAM3-COD-SQL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO
                                       TO WAM3-COD-MSG-ERRO
HEXA           MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                                          WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE   EQUAL 1
               MOVE 9                  TO WAM3-ERRO
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
                                          WAM3-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5296'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MONTA AREA DE SAIDA                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO WAM3-LL.
           MOVE WAM2-ZZ                TO WAM3-ZZ.

           MOVE WAM2-TRANSACAO         TO WAM3-TRANSACAO.
           MOVE WAM2-FUNCAO            TO WAM3-FUNCAO.
           MOVE WAM2-TERMINAL          TO WAM3-TERMINAL.
           MOVE WAM2-FUNC-BDSCO        TO WAM3-FUNC-BDSCO.
           MOVE WAM2-FIXO              TO WAM3-FIXO.

           MOVE 'S'                    TO WAM3-FIM.

           MOVE ZEROS                  TO WAM3-ERRO
                                          WAM3-COD-SQL-ERRO
                                          WAM3-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO WAM3-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONSISTE DADOS                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAM2-TRANSACAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAM2-FUNCAO             EQUAL SPACES OR LOW-VALUES) OR
              (WAM2-FUNC-BDSCO         EQUAL SPACES OR LOW-VALUES) OR
              (WAM2-CAGENCIA           EQUAL ZEROS) OR
              (WAM2-CPAB               EQUAL ZEROS) OR
              (WAM2-CAGPTO             NOT EQUAL 17 AND 20 )
               MOVE  1                 TO WAM3-ERRO
               MOVE  ZEROS             TO WAM3-COD-SQL-ERRO
               MOVE  0001              TO WAM3-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAM3-FIM
               PERFORM 1310-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTEM DESCRICAO DA MENSAGEM                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAM2-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAM2-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAM2-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAM3-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
                                          WAM3-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5296'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WAM3-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
                                          WAM3-DESC-MSG-ERRO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5296'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                  TO WAM3-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAM3-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAM3-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-CONSISTIR-AGENCIA.

           PERFORM 2200-CONS-POSTAL-EXPRESSO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONSISTIR AGENCIA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO COMU-BANCO.
           MOVE WAM2-CAGENCIA          TO COMU-DEPDC.
           MOVE 'GFCT5296'             TO COMU-NOME-PGM.
           MOVE 'MESU9015'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING COMU-PARM
                                          WRK-ERRO-AREA
                                          WRK-SQLCA.

           IF  COMU-COD-RETORNO       NOT EQUAL ZEROS
               MOVE 1                  TO WAM3-ERRO
               MOVE 0465               TO WAM3-COD-MSG-ERRO
               MOVE '0465'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO WAM3-COD-SQL-ERRO
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CONSISTIR BANCO POSTAL E BRADESCO EXPRESSO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONS-POSTAL-EXPRESSO       SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO COMU-VERSAO
           MOVE 237                    TO COMU-COD-EMPR
           MOVE WAM2-CAGENCIA          TO COMU-COD-DEPDC
           MOVE WAM2-CPAB              TO COMU-COD-POSTO
           MOVE SPACES                 TO COMU-CORR-BANCARIO
0610       MOVE SPACES                 TO WAM3-DESC-POSTO

           MOVE 'MESU9018'             TO WRK-MODULO

           CALL  WRK-MODULO         USING COMU-DADOS-9018

           IF  RETURN-CODE      NOT EQUAL ZEROS
               IF  RETURN-CODE      EQUAL 4
                   MOVE 1              TO WAM3-ERRO
                   MOVE 0518           TO WAM3-COD-MSG-ERRO
                   MOVE '0518'         TO GFCTG2-COD-MSG
                   MOVE ZEROS          TO WAM3-COD-SQL-ERRO
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
                   IF  RETURN-CODE   EQUAL 8
                       MOVE 1          TO WAM3-ERRO
                       MOVE 0519       TO WAM3-COD-MSG-ERRO
                       MOVE '0519'     TO GFCTG2-COD-MSG
                       MOVE ZEROS      TO WAM3-COD-SQL-ERRO
                       PERFORM 1310-OBTER-DESC-MSG
                       MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                       PERFORM 3000-FINALIZAR
                   ELSE
                       IF  RETURN-CODE   EQUAL  12
                           MOVE 1      TO WAM3-ERRO
                           MOVE 0520   TO WAM3-COD-MSG-ERRO
                           MOVE '0520' TO GFCTG2-COD-MSG
                           MOVE ZEROS  TO WAM3-COD-SQL-ERRO
                           PERFORM 1310-OBTER-DESC-MSG
                           MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                           PERFORM 3000-FINALIZAR
                       END-IF
                   END-IF
               END-IF
           ELSE
              IF  COMU-COD-TIPO-POSTO  NOT EQUAL 9
                  IF WAM2-CAGPTO EQUAL 17
                      MOVE 1           TO WAM3-ERRO
                      MOVE 0521        TO WAM3-COD-MSG-ERRO
                      MOVE '0521'      TO GFCTG2-COD-MSG
                      MOVE ZEROS       TO WAM3-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  ELSE
                      MOVE 1           TO WAM3-ERRO
                      MOVE 1480        TO WAM3-COD-MSG-ERRO
                      MOVE '1480'      TO GFCTG2-COD-MSG
                      MOVE ZEROS       TO WAM3-COD-SQL-ERRO
                      PERFORM 1310-OBTER-DESC-MSG
                      MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                      PERFORM 3000-FINALIZAR
                  END-IF
              END-IF
           END-IF.

           PERFORM 2210-OBTER-DADOS-YMKCB001.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSO A TABELA YMKCB001                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OBTER-DADOS-YMKCB001       SECTION.
      *----------------------------------------------------------------*
           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01                     TO KCIMW184-E-FUNCAO.
           MOVE WAM2-CAGENCIA          TO KCIMW184-E-CAG-BCRIA-1.
           MOVE WAM2-CPAB              TO WRK-CPAB-9-05.
0610       MOVE SPACES                 TO WAM3-DESC-POSTO
           MOVE WRK-CPAB-9-03          TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO    USING   KCIMW184-AREA-COMUNICACAO.

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
               MOVE 1                  TO WAM3-ERRO
               MOVE ZEROS              TO WAM3-COD-SQL-ERRO
               MOVE 1875               TO WAM3-COD-MSG-ERRO
               MOVE '1875'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  KCIMW184-S-COD-RETORNO EQUAL 98
               IF  WAM2-CAGPTO      EQUAL 17
                   MOVE 1              TO WAM3-ERRO
                   MOVE ZEROS          TO WAM3-COD-SQL-ERRO
                   MOVE 0521           TO WAM3-COD-MSG-ERRO
                   MOVE '0521'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE 1              TO WAM3-ERRO
                   MOVE ZEROS          TO WAM3-COD-SQL-ERRO
                   MOVE 1480           TO WAM3-COD-MSG-ERRO
                   MOVE '1480'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO WAM3-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  KCIMW184-S-CEMPR-CORSP(1)   NOT EQUAL  10000  AND
               WAM2-CAGPTO          EQUAL 17
               MOVE 1                  TO WAM3-ERRO
               MOVE ZEROS              TO WAM3-COD-SQL-ERRO
               MOVE 0521               TO WAM3-COD-MSG-ERRO
               MOVE '0521'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  KCIMW184-S-CEMPR-CORSP(1)   EQUAL  10000  AND
               WAM2-CAGPTO          EQUAL 20
               MOVE 1                  TO WAM3-ERRO
               MOVE ZEROS              TO WAM3-COD-SQL-ERRO
               MOVE 1480               TO WAM3-COD-MSG-ERRO
               MOVE '1480'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAM3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE KCIMW184-S-ICORSP-BCRIO(1)
                                       TO WRK-DESC-POSTO-70.
           MOVE WRK-DESC-POSTO-20      TO WAM3-DESC-POSTO.

      *---------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *RETORNA AO CHAMADOR                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
