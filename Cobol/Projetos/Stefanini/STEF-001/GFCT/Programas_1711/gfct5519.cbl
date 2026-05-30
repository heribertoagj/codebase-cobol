      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5519.
       AUTHOR.     PROCWORK OUTSOURCING.
      *----------------------------------------------------------------*
      *                    P  R  O  C  W  O  R  K                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5519                                     *
      *    PROGRAMADOR  : MARCUS VINICIUS               - PROCWORK     *
      *    ANALISTA     : MARCUS VINICIUS - PROCWORK    - GRUPO 50     *
      *    DATA         : 12/09/2008                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      ATRAVES DE UMA AGENCIA E POSTO BANCARIO, RETORNA SE       *
      *      EH PAB, BANCO POSTAL OU BRADESCO EXPRESSO.                *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.POSTO_BCRIO_SERVC               MESUB062          *
      *       DB2PRD.TCAD_CORSP_BCRIO                YMKCB001          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL1050 - CONEXAO COM DB2                                *
      *      POOL7100 - TRATAMENTO DE ERROS DE APLICACAO               *
      *                                                                *
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
      *                                                                *
      *    ANALISTA....:  CIBELE          - SONDA IT                   *
      *    DATA........:  05 / 2014                                    *
      *    OBJETIVO....:  SUBSTITUIR O ACESSO A BASE YMKCB001          *
      *                   PELO MODULO KCIM0184.                        *
      *                                                                *
      *================================================================*
      *
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
           '*  INICIO DA WORKING GFCT5519  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO             PIC  X(008)         VALUE SPACES.

       01  WRK-SQL-CODE-N              PIC S9(09)   VALUE ZEROES.
       01  WRK-SQL-CODE-R  REDEFINES  WRK-SQL-CODE-N.
           03  FILLER                  PIC  9(06).
           03  WRK-SQL-CODE            PIC  9(03).

       01  WRK-CPAB-9-05              PIC  9(005)         VALUE ZEROS.
       01  WRK-FILLER                 REDEFINES WRK-CPAB-9-05.
           05 FILLER                  PIC  9(02).
           05 WRK-CPAB-9-03           PIC  9(03).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE TABELAS DB2      *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE MESUB062
           END-EXEC.
HEXA  *
      *---------------------------------------------------------------*
      * AREA DE OMUNICACAO COM O MODULO KCIM0184                      *
      *---------------------------------------------------------------*

           COPY KCIMW184.

HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
HEXA       COPY I#BRAD7C.
HEXA  *

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5519   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWABE'.

       01 LNK-IO-PCB.
           03 LNK-IO-LTERM           PIC X(08).
           03 FILLER                 PIC X(02).
           03 LNK-IO-STATUS          PIC X(02).
           03 FILLER                 PIC X(12).
           03 LNK-IO-MODNAME         PIC X(08).

       01 LNK-ALT-PCB.
           03 LNK-ALT-LTERM          PIC X(08).
           03 FILLER                 PIC X(02).
           03 LNK-ALT-STATUS         PIC X(02).
           03 FILLER                 PIC X(12).
           03 LNK-ALT-MODNAME        PIC X(08).

      *================================================================*
       PROCEDURE DIVISION              USING WABE-AREA-GFCT5519
                                             LNK-IO-PCB
                                             LNK-ALT-PCB.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-INICIALIZAR-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WABE-AREA-SAIDA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF WABE-AMBIENTE      NOT EQUAL 'B' AND 'O'
               MOVE 01               TO WABE-COD-RETORNO
               MOVE 'AMBIENTE DIFERENTE DE B/O'
                                       TO WABE-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

           IF WABE-CJUNC-DEPDC       NOT NUMERIC        OR
               WABE-CPOSTO-SERVC     NOT NUMERIC
               MOVE 02               TO WABE-COD-RETORNO
               MOVE 'CAMPOS INCONSISTENTES - AGENCIA OU PAB'
                                       TO WABE-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  * HX_IF  WABE-CJUNC-DEPDC    GREATER 3999
HEXA  *
HEXA       MOVE        WABE-CJUNC-DEPDC TO     WRK-MESUX0-AGENCIA
HEXA       PERFORM     9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA       IF  WRK-NAO-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
               MOVE 03                 TO WABE-COD-RETORNO
               MOVE 'CODIGO DE JUNCAO/DEPTO DEVE SER MENOR QUE 4000'
                                       TO WABE-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WABE-AMBIENTE       EQUAL 'B'
               CALL 'POOL1050'
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-PROCESSAR-MESU.

      *----------------------------------------------------------------*
       2000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-PROCESSAR-MESU            SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO CEMPR-INC    OF MESUB062.
           MOVE WABE-CJUNC-DEPDC       TO CDEPDC       OF MESUB062.
           MOVE WABE-CPOSTO-SERVC      TO CPOSTO-SERVC OF MESUB062.

           PERFORM 2110-LER-MESUB062.

           IF  SQLCODE                 EQUAL +100
               MOVE 04                 TO WABE-COD-RETORNO
               MOVE 'REGISTRO NAO ENCONTRADO MESU'
                                       TO WABE-DESC-MSG
           ELSE
               IF  CTPO-POSTO-SERVC OF MESUB062
                                       NOT EQUAL 9
                   MOVE IPOSTO-SERVC OF MESUB062
                                       TO WABE-IPOSTO-SERVC
                   MOVE 1
                                       TO WABE-TIPO-POSTO
               ELSE
                   PERFORM 2200-PROCESSAR-YMKC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-LER-MESUB062              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT CPOSTO_SERVC,
                      CTPO_POSTO_SERVC,
                      IPOSTO_SERVC
               INTO  :MESUB062.CPOSTO-SERVC,
                     :MESUB062.CTPO-POSTO-SERVC,
                     :MESUB062.IPOSTO-SERVC
               FROM  DB2PRD.POSTO_BCRIO_SERVC
               WHERE CEMPR_INC          = :MESUB062.CEMPR-INC
                 AND CDEPDC             = :MESUB062.CDEPDC
                 AND CPOSTO_SERVC       = :MESUB062.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 99                 TO WABE-COD-RETORNO
               MOVE SQLCODE            TO WRK-SQL-CODE-N
               MOVE WRK-SQL-CODE       TO WABE-COD-SQL-ERRO
               MOVE 'ERRO NO MODULO GFCT5519 - MESUB062'
                                       TO WABE-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-YMKC            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE KCIMW184-AREA-COMUNICACAO.

           MOVE 01                     TO KCIMW184-E-FUNCAO.
           MOVE WABE-CJUNC-DEPDC       TO KCIMW184-E-CAG-BCRIA-1.
           MOVE WABE-CPOSTO-SERVC      TO WRK-CPAB-9-05.
           MOVE WRK-CPAB-9-03          TO KCIMW184-E-CPOSTO-BCRIO-1.
           MOVE 'KCIM0184'             TO WRK-MODULO.

           CALL WRK-MODULO             USING KCIMW184-AREA-COMUNICACAO.

           PERFORM 2210-LER-YMKCB001.

           IF  KCIMW184-S-COD-RETORNO EQUAL 98
               MOVE 05                 TO WABE-COD-RETORNO
               MOVE 'REGISTRO NAO ENCONTRADO YMKC'
                                       TO WABE-DESC-MSG
           ELSE
               IF  KCIMW184-S-CEMPR-CORSP(1)
                                       EQUAL 10000
                   MOVE IPOSTO-SERVC OF MESUB062
                                       TO WABE-IPOSTO-SERVC
                   MOVE 2
                                       TO WABE-TIPO-POSTO
               ELSE
                   MOVE IPOSTO-SERVC OF MESUB062
                                       TO WABE-IPOSTO-SERVC
                   MOVE 3
                                       TO WABE-TIPO-POSTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-LER-YMKCB001               SECTION.
      *----------------------------------------------------------------*

           IF KCIMW184-S-COD-RETORNO  NOT EQUAL 00 AND 98
               MOVE KCIMW184-S-COD-RETORNO     TO WABE-COD-RETORNO
               MOVE 'ERRO NO MODULO KCIM0184 ' TO WABE-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
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
HEXA       MOVE    'GFCT5519'      TO          ERR-PGM.
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
