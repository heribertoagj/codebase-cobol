      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVE039B.
       AUTHOR.     MARIA GORETI STOPASOLLA.
      *================================================================*
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE039B                                      *
      *    PROGRAMADOR.: MARIA GORETI STOPASOLLA - CD CURITIBA         *
      *    DATA........: 17/08/2018                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: MODULO PARA REALIZAR MANUTENCAO NA TABELA     *
      *                  TCRDRA_ARRJO_PGTO_LCTO, DE ACORDO COM A FUNCAO*
      *                  INFORMADA.                                    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TARRJO_PGTO                                 BVVEB038        *
      *    TCRDRA_ARRJO_PGTO_LCTO                      BVVEB039        *
      *    TCRDRA_ESTBL_COML                           BVVEB040        *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    BVVEW39I - BOOK DE ENTRADA                                  *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE FRWK2999 (LOG DE ERRO)                             *
      *    I#FRWKHE FRWK2999 (AREA COMUM ERROS)                        *
      *    I#FRWKDB FRWK2999 (LOG DE ERROS DB2 )                       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
      *----------------------------------------------------------------*
      * O P C O E S   D E   E N T R A D A :                            *
      *--------------+---------------------+---------------------------*
      * BVVEW39I-    |  CAMPOS REQUERIDOS  |OBSERVACOES                *
      * E-OPCAO      |  BVVEW39I-E-        |                           *
      *--------------+---------------------+---------------------------*
      *    '001'     |  CPSSOA-JURID-PRINC |RETORNA TODOS OS DADOS COM *
      *              |  CTPO-CONTR-PRINC   |BASE NOS CAMPOS REQUERIDOS *
      *              |  NSEQ-CTA-PRINC     |                           *
      *================================================================*
      * ALTERACOES                                                     *
      *--------------+-------------------------------------------------*
      * DATA         |  AUTOR                                          *
      * DESCRICAO                                                      *
      *--------------+-------------------------------------------------*
181204* 04/12/2018   |  SIVEK                                          *
      * INCLUIR PESQUISA PARA CRITERIOS 9999                           *
      *--------------+---------------------+---------------------------*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING BVVE039B'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008)         VALUE
              'BVVE039B'.
           05 WRK-TABELA-01            PIC  X(032)         VALUE
              'TCRDRA_ARRJO_PGTO_LCTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE
           'AREA PARA VARIAVEIS NULAS'.
      *----------------------------------------------------------------*
       01  WRK-BVVEB039-NULL.
           03 WRK-HINCL-REG-NULL       PIC S9(004) COMP    VALUE +0.
           03 WRK-CUSUAR-INCL-NULL     PIC S9(004) COMP    VALUE +0.
           03 WRK-HMANUT-REG-NULL      PIC S9(004) COMP    VALUE +0.
           03 WRK-CUSUAR-MANUT-NULL    PIC S9(004) COMP    VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE TRATAMENTO DE ERRO DB2 (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB038
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB039
           END-EXEC.

           EXEC SQL
               INCLUDE BVVEB040
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)      VALUE
           'FIM DA WORKING BVVE039B'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
       01 LNK-BVVE039B.
           COPY BVVEW39I.
           COPY BVVEW000.

      **===============================================================*
       PROCEDURE DIVISION   USING LNK-BVVE039B.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           CALL    'CKRS1000'.
           CALL    'CKRS1050'.

           MOVE ZEROS                  TO BVVEW000-COD-RETORNO.
           MOVE '0000'                 TO BVVEW000-COD-ERRO.
           MOVE 'BVVE0001'             TO BVVEW000-COD-MENSAGEM.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 8000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA.                                 *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGERR-REGISTRO
                      FRWKGDB2-REGISTRO
            REPLACING ALPHANUMERIC BY SPACES
                           NUMERIC BY ZEROS

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.              *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*


           IF (BVVEW39I-E-C-ATIVIDADE  NOT EQUAL 'I' AND 'C' AND 'A'
                                       AND 'D')
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           IF (BVVEW39I-E-C-FUNCAO     NOT EQUAL '001')
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           EVALUATE BVVEW39I-E-C-ATIVIDADE
               WHEN 'C'
               WHEN 'D'
                    PERFORM 1110-CONSISTE-CHAVE
               WHEN 'I'
               WHEN 'A'
                    PERFORM 1120-CONSISTE-CAMPOS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTENCIA DE DADOS PARA A CONSULTA.                      *
      *----------------------------------------------------------------*
       1110-CONSISTE-CHAVE             SECTION.
      *----------------------------------------------------------------*

           IF (BVVEW39I-E-CCRDRA-ESTBL-COML
                                       NOT NUMERIC)
           OR (BVVEW39I-E-CCRDRA-ESTBL-COML
                                       EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1111-VERIFICAR-CREDENCIADORA

           IF (BVVEW39I-E-CARRJO-PGTO  NOT NUMERIC)
           OR (BVVEW39I-E-CARRJO-PGTO  EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1112-VERIFICAR-ARRANJO-PGTO

           IF (BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       NOT NUMERIC)
           OR (BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA: CONSULTAR TABELA BVVEB040                         *
      *----------------------------------------------------------------*
       1111-VERIFICAR-CREDENCIADORA    SECTION.
      *----------------------------------------------------------------*


           INITIALIZE BVVEB040         REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE BVVEW39I-E-CCRDRA-ESTBL-COML
                                       TO CCRDRA-ESTBL-COML OF BVVEB040

           EXEC SQL
               SELECT  CCRDRA_ESTBL_COML
                 INTO :BVVEB040.CCRDRA-ESTBL-COML
                 FROM DB2PRD.TCRDRA_ESTBL_COML
                WHERE CCRDRA_ESTBL_COML  = :BVVEB040.CCRDRA-ESTBL-COML
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF (SQLCODE                      NOT EQUAL ZEROS AND +100)
               SET DB2-SELECT               TO TRUE
               MOVE 12                      TO BVVEW000-COD-RETORNO
               MOVE '0040'                  TO BVVEW000-COD-ERRO
               MOVE 'BVVE0012'              TO BVVEW000-COD-MENSAGEM
               MOVE WRK-TABELA-01           TO FRWKGDB2-NOME-TABELA
               MOVE '1111-VERIFICAR-CREDENCIADORA'
                                            TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9200-ERRO-DB2
           END-IF.

           IF (SQLCODE                      EQUAL +100)
               MOVE  10                     TO BVVEW000-COD-RETORNO
               MOVE '0050'                  TO BVVEW000-COD-ERRO
               MOVE 'BVVE0016'              TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA: CONSULTAR TABELA BVVEB038                         *
      *----------------------------------------------------------------*
       1112-VERIFICAR-ARRANJO-PGTO     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEB038         REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE BVVEW39I-E-CARRJO-PGTO TO CARRJO-PGTO OF BVVEB038

           EXEC SQL
               SELECT  CARRJO_PGTO
                 INTO :BVVEB038.CARRJO-PGTO
                 FROM DB2PRD.TARRJO_PGTO
                WHERE CARRJO_PGTO = :BVVEB038.CARRJO-PGTO
                FETCH FIRST 1 ROW ONLY
           END-EXEC

           IF (SQLCODE                      NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                     EQUAL     'W')
               SET DB2-SELECT               TO TRUE
               MOVE 12                      TO BVVEW000-COD-RETORNO
               MOVE '0040'                  TO BVVEW000-COD-ERRO
               MOVE 'BVVE0012'              TO BVVEW000-COD-MENSAGEM
               MOVE WRK-TABELA-01           TO FRWKGDB2-NOME-TABELA
               MOVE '1112-VERIFICAR-ARRANJO-PGTO'
                                            TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9200-ERRO-DB2
           END-IF.

           IF (SQLCODE                      EQUAL +100)
               MOVE  10                     TO BVVEW000-COD-RETORNO
               MOVE '0050'                  TO BVVEW000-COD-ERRO
               MOVE 'BVVE0016'              TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSISTENCIA DE DADOS PARA A INCLUSAO.                      *
      *----------------------------------------------------------------*
       1120-CONSISTE-CAMPOS            SECTION.
      *----------------------------------------------------------------*

           IF (BVVEW39I-E-CCRDRA-ESTBL-COML
                                       NOT NUMERIC)
           OR (BVVEW39I-E-CCRDRA-ESTBL-COML
                                       EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1111-VERIFICAR-CREDENCIADORA

           IF (BVVEW39I-E-CARRJO-PGTO  NOT NUMERIC)
           OR (BVVEW39I-E-CARRJO-PGTO  EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1112-VERIFICAR-ARRANJO-PGTO

           IF (BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       NOT NUMERIC)
           OR (BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           IF (BVVEW39I-E-CIDTFD-LCTO  NOT NUMERIC)
           OR (BVVEW39I-E-CIDTFD-LCTO  EQUAL ZEROS)
               MOVE 08                 TO BVVEW000-COD-RETORNO
               MOVE '0001'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

           IF (BVVEW39I-E-C-ATIVIDADE  EQUAL 'I')
               IF (BVVEW39I-E-CUSUAR-INCL
                                       EQUAL SPACES)
               OR (BVVEW39I-E-CUSUAR-INCL
                                       EQUAL LOW-VALUES)
                   MOVE 08                 TO BVVEW000-COD-RETORNO
                   MOVE '0001'             TO BVVEW000-COD-ERRO
                   MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
                   PERFORM 8000-FINALIZAR
               END-IF
           ELSE
               IF (BVVEW39I-E-CUSUAR-MANUT
                                       EQUAL SPACES)
               OR (BVVEW39I-E-CUSUAR-MANUT
                                       EQUAL LOW-VALUES)
                   MOVE 08                 TO BVVEW000-COD-RETORNO
                   MOVE '0001'             TO BVVEW000-COD-ERRO
                   MOVE 'BVVE0004'         TO BVVEW000-COD-MENSAGEM
                   PERFORM 8000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE BVVEW39I-E-C-ATIVIDADE
               WHEN 'C'
                    PERFORM 2100-CONSULTA-BVVEB039
               WHEN 'I'
                    PERFORM 2200-INCLUI-BVVEB039
               WHEN 'A'
                    PERFORM 2300-ALTERA-BVVEB039
               WHEN 'D'
                    PERFORM 2400-DELETA-BVVEB039
               WHEN OTHER
                    MOVE 16                TO BVVEW000-COD-RETORNO
                    MOVE '0070'            TO BVVEW000-COD-ERRO
                    MOVE 'BVVE0004'        TO BVVEW000-COD-MENSAGEM
                    PERFORM 8000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA: CONSULTAR TABELA BVVEB039                         *
      *----------------------------------------------------------------*
       2100-CONSULTA-BVVEB039          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEB039         REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE BVVEW39I-E-CCRDRA-ESTBL-COML
                                       TO CCRDRA-ESTBL-COML OF BVVEB039
           MOVE BVVEW39I-E-CARRJO-PGTO TO CARRJO-PGTO       OF BVVEB039
           MOVE BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       TO NPRFIL-CRDRA-PGTO OF BVVEB039

           EXEC SQL
               SELECT  CCRDRA_ESTBL_COML
                      ,CARRJO_PGTO
                      ,NPRFIL_CRDRA_PGTO
                      ,CIDTFD_LCTO
                      ,HINCL_REG
                      ,CUSUAR_INCL
                      ,HMANUT_REG
                      ,CUSUAR_MANUT
                 INTO :BVVEB039.CCRDRA-ESTBL-COML
                     ,:BVVEB039.CARRJO-PGTO
                     ,:BVVEB039.NPRFIL-CRDRA-PGTO
                     ,:BVVEB039.CIDTFD-LCTO
                     ,:BVVEB039.HINCL-REG
                           :WRK-HINCL-REG-NULL
                     ,:BVVEB039.CUSUAR-INCL
                           :WRK-CUSUAR-INCL-NULL
                     ,:BVVEB039.HMANUT-REG
                           :WRK-HMANUT-REG-NULL
                     ,:BVVEB039.CUSUAR-MANUT
                           :WRK-CUSUAR-MANUT-NULL
                 FROM DB2PRD.TCRDRA_ARRJO_PGTO_LCTO
                WHERE CCRDRA_ESTBL_COML  = :BVVEB039.CCRDRA-ESTBL-COML
                  AND CARRJO_PGTO        = :BVVEB039.CARRJO-PGTO
                  AND NPRFIL_CRDRA_PGTO  = :BVVEB039.NPRFIL-CRDRA-PGTO
                FETCH FIRST 1 ROW ONLY
           END-EXEC


           IF (SQLCODE                      NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                     EQUAL     'W')
               SET DB2-SELECT               TO TRUE
               MOVE 12                      TO BVVEW000-COD-RETORNO
               MOVE '0040'                  TO BVVEW000-COD-ERRO
               MOVE 'BVVE0012'              TO BVVEW000-COD-MENSAGEM
               MOVE WRK-TABELA-01           TO FRWKGDB2-NOME-TABELA
               MOVE '2100-CONSULTA-BVVEB039' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9200-ERRO-DB2
           END-IF.

           IF (SQLCODE                 EQUAL +100)
181204         INITIALIZE BVVEB039     REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.              MOVE BVVEW39I-E-CCRDRA-ESTBL-COML
.                                      TO CCRDRA-ESTBL-COML OF BVVEB039
.              MOVE 999                TO CARRJO-PGTO       OF BVVEB039
181204
               PERFORM 2120-VERIFICA-OUTROS
               IF (SQLCODE             EQUAL +100)
181204             INITIALIZE BVVEB039 REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.                  MOVE 9999           TO CCRDRA-ESTBL-COML OF BVVEB039
.                  MOVE BVVEW39I-E-CARRJO-PGTO
.                                      TO CARRJO-PGTO       OF BVVEB039
.                  PERFORM 2120-VERIFICA-OUTROS
.                  IF (SQLCODE         EQUAL +100)
.                      INITIALIZE BVVEB039
.                                      REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.                      MOVE 9999       TO CCRDRA-ESTBL-COML OF BVVEB039
.                      MOVE 999        TO CARRJO-PGTO       OF BVVEB039
.                      PERFORM 2120-VERIFICA-OUTROS
.                      IF (SQLCODE         EQUAL +100)
.                          MOVE  10    TO BVVEW000-COD-RETORNO
.                          MOVE '0050' TO BVVEW000-COD-ERRO
.                          MOVE 'BVVE0016'
.                                      TO BVVEW000-COD-MENSAGEM
.                          PERFORM 8000-FINALIZAR
.                      END-IF
181204             END-IF
               END-IF
           END-IF.

           PERFORM 2150-TRATAR-NULOS-CONSULTA.

           PERFORM 5000-MOVER-DADOS-SAIDA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-VERIFICA-OUTROS            SECTION.
      *----------------------------------------------------------------*
           MOVE BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       TO NPRFIL-CRDRA-PGTO OF BVVEB039

           EXEC SQL
               SELECT  CCRDRA_ESTBL_COML
                      ,CARRJO_PGTO
                      ,NPRFIL_CRDRA_PGTO
                      ,CIDTFD_LCTO
                      ,HINCL_REG
                      ,CUSUAR_INCL
                      ,HMANUT_REG
                      ,CUSUAR_MANUT
                 INTO :BVVEB039.CCRDRA-ESTBL-COML
                     ,:BVVEB039.CARRJO-PGTO
                     ,:BVVEB039.NPRFIL-CRDRA-PGTO
                     ,:BVVEB039.CIDTFD-LCTO
                     ,:BVVEB039.HINCL-REG
                           :WRK-HINCL-REG-NULL
                     ,:BVVEB039.CUSUAR-INCL
                           :WRK-CUSUAR-INCL-NULL
                     ,:BVVEB039.HMANUT-REG
                           :WRK-HMANUT-REG-NULL
                     ,:BVVEB039.CUSUAR-MANUT
                           :WRK-CUSUAR-MANUT-NULL
                 FROM DB2PRD.TCRDRA_ARRJO_PGTO_LCTO
                WHERE CCRDRA_ESTBL_COML  = :BVVEB039.CCRDRA-ESTBL-COML
                  AND CARRJO_PGTO        = :BVVEB039.CARRJO-PGTO
                  AND NPRFIL_CRDRA_PGTO  = :BVVEB039.NPRFIL-CRDRA-PGTO
                FETCH FIRST 1 ROW ONLY
           END-EXEC.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR TRATAR NULOS NA CONSULTA.            *
      *----------------------------------------------------------------*
       2150-TRATAR-NULOS-CONSULTA      SECTION.
      *----------------------------------------------------------------*

           IF (WRK-HINCL-REG-NULL      EQUAL -1)
               MOVE '0001-01-01-01.01.01.000001'
                                       TO HINCL-REG          OF BVVEB039
           END-IF

           IF (WRK-CUSUAR-INCL-NULL    EQUAL -1)
               MOVE SPACES             TO CUSUAR-INCL        OF BVVEB039
           END-IF

           IF (WRK-HMANUT-REG-NULL     EQUAL -1)
               MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG         OF BVVEB039
           END-IF

           IF (WRK-CUSUAR-MANUT-NULL   EQUAL -1)
               MOVE SPACES             TO CUSUAR-MANUT       OF BVVEB039
           END-IF.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   ROTINA RESPONSAVEL POR INCLUIR REGISTRO NA TABELA BVVEB039   *
      *----------------------------------------------------------------*
       2200-INCLUI-BVVEB039            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2500-MOVER-DADOS-ENTRADA

           EXEC SQL
                   INSERT INTO DB2PRD.TCRDRA_ARRJO_PGTO_LCTO
                         (CCRDRA_ESTBL_COML
                         ,CARRJO_PGTO
                         ,NPRFIL_CRDRA_PGTO
                         ,CIDTFD_LCTO
                         ,HINCL_REG
                         ,CUSUAR_INCL
                         ,HMANUT_REG
                         ,CUSUAR_MANUT)
                   VALUES
                         (:BVVEB039.CCRDRA-ESTBL-COML
                         ,:BVVEB039.CARRJO-PGTO
                         ,:BVVEB039.NPRFIL-CRDRA-PGTO
                         ,:BVVEB039.CIDTFD-LCTO
                         , CURRENT TIMESTAMP
                         ,:BVVEB039.CUSUAR-INCL
                               :WRK-CUSUAR-INCL-NULL
                         ,:BVVEB039.HMANUT-REG
                               :WRK-HMANUT-REG-NULL
                         ,:BVVEB039.CUSUAR-MANUT
                               :WRK-CUSUAR-MANUT-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0080'             TO BVVEW000-COD-ERRO
               MOVE WRK-TABELA-01      TO FRWKGDB2-NOME-TABELA
               MOVE '2200-INCLUI-BVVEB039'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9200-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
       2300-ALTERA-BVVEB039            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2500-MOVER-DADOS-ENTRADA

           EXEC SQL
               UPDATE  DB2PRD.TCRDRA_ARRJO_PGTO_LCTO
                  SET  CIDTFD_LCTO       = :BVVEB039.CIDTFD-LCTO
                      ,CUSUAR_INCL       = :BVVEB039.CUSUAR-INCL
                      ,HMANUT_REG        =  CURRENT TIMESTAMP
                WHERE  CCRDRA_ESTBL_COML = :BVVEB039.CCRDRA-ESTBL-COML
                  AND  CARRJO_PGTO       = :BVVEB039.CARRJO-PGTO
                  AND  NPRFIL_CRDRA_PGTO = :BVVEB039.NPRFIL-CRDRA-PGTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL     'W')
               SET DB2-UPDATE           TO TRUE
               MOVE 12                  TO BVVEW000-COD-RETORNO
               MOVE '0100'              TO BVVEW000-COD-ERRO
               MOVE 'BVVE0017'          TO BVVEW000-COD-MENSAGEM
               MOVE WRK-TABELA-01       TO FRWKGDB2-NOME-TABELA
               MOVE '2300-ALTERA-BVVEB039'
                                        TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9200-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR REALIZAR A EXCLUSAO NA TABELA BVVEB039  *
      *----------------------------------------------------------------*
       2400-DELETA-BVVEB039            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEB039         REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE BVVEW39I-E-CCRDRA-ESTBL-COML
                                       TO CCRDRA-ESTBL-COML OF BVVEB039
           MOVE BVVEW39I-E-CARRJO-PGTO TO CARRJO-PGTO       OF BVVEB039
           MOVE BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       TO NPRFIL-CRDRA-PGTO OF BVVEB039

           EXEC SQL
                DELETE
                  FROM DB2PRD.TCRDRA_ARRJO_PGTO_LCTO
                 WHERE CCRDRA_ESTBL_COML = :BVVEB039.CCRDRA-ESTBL-COML
                   AND CARRJO_PGTO       = :BVVEB039.CARRJO-PGTO
                   AND NPRFIL_CRDRA_PGTO = :BVVEB039.NPRFIL-CRDRA-PGTO
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-DELETE          TO TRUE
               MOVE 12                 TO BVVEW000-COD-RETORNO
               MOVE '0310'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0008'         TO BVVEW000-COD-MENSAGEM
               MOVE WRK-TABELA-01      TO FRWKGDB2-NOME-TABELA
               MOVE '2400-DELETA-BVVEB039'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9200-ERRO-DB2
           END-IF

           IF (SQLCODE                 EQUAL +100)
               MOVE 10                 TO BVVEW000-COD-RETORNO
               MOVE '0320'             TO BVVEW000-COD-ERRO
               MOVE 'BVVE0008'         TO BVVEW000-COD-MENSAGEM
               PERFORM 8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FORMATAR A AREA DE INCLUSAO.         *
      *----------------------------------------------------------------*
       2500-MOVER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEW39I-E-CCRDRA-ESTBL-COML
                                       TO CCRDRA-ESTBL-COML  OF BVVEB039
           MOVE BVVEW39I-E-CARRJO-PGTO TO CARRJO-PGTO        OF BVVEB039
           MOVE BVVEW39I-E-NPRFIL-CRDRA-PGTO
                                       TO NPRFIL-CRDRA-PGTO  OF BVVEB039
           MOVE BVVEW39I-E-CIDTFD-LCTO TO CIDTFD-LCTO        OF BVVEB039

           IF (BVVEW39I-E-C-ATIVIDADE  EQUAL 'I')
               MOVE BVVEW39I-E-CUSUAR-INCL
                                       TO CUSUAR-INCL        OF BVVEB039
           ELSE
               MOVE BVVEW39I-E-CUSUAR-MANUT
                                       TO CUSUAR-MANUT       OF BVVEB039
           END-IF

           PERFORM 2600-TRATAR-NULOS.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR TRATAR NULOS NA INCLUSAO.            *
      *----------------------------------------------------------------*
       2600-TRATAR-NULOS               SECTION.
      *----------------------------------------------------------------*

           IF (CUSUAR-INCL             OF BVVEB039 EQUAL SPACES)
               MOVE -1                 TO WRK-CUSUAR-INCL-NULL
           END-IF

           IF (HINCL-REG               OF BVVEB039 EQUAL SPACES)
           OR (HINCL-REG               OF BVVEB039 EQUAL LOW-VALUES)
               MOVE '0001-01-01-00.00.00.000000'
                                       TO HINCL-REG         OF BVVEB039
               MOVE -1                 TO WRK-HINCL-REG-NULL
           END-IF

           IF (CUSUAR-MANUT            OF BVVEB039 EQUAL SPACES)
               MOVE -1                 TO WRK-CUSUAR-MANUT-NULL
           END-IF

           IF (HMANUT-REG              OF BVVEB039 EQUAL SPACES)
           OR (HMANUT-REG              OF BVVEB039 EQUAL LOW-VALUES)
               MOVE '0001-01-01-00.00.00.000000'
                                       TO HMANUT-REG        OF BVVEB039
               MOVE -1                 TO WRK-HMANUT-REG-NULL
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR FORMATAR A AREA DE SAIDA.            *
      *----------------------------------------------------------------*
       5000-MOVER-DADOS-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVEW39I-SAIDA  REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE CCRDRA-ESTBL-COML      OF BVVEB039
                                       TO BVVEW39I-S-CCRDRA-ESTBL-COML
           MOVE CARRJO-PGTO            OF BVVEB039
                                       TO BVVEW39I-S-CARRJO-PGTO
           MOVE NPRFIL-CRDRA-PGTO      OF BVVEB039
                                       TO BVVEW39I-S-NPRFIL-CRDRA-PGTO
           MOVE CIDTFD-LCTO            OF BVVEB039
                                       TO BVVEW39I-S-CIDTFD-LCTO
           MOVE HINCL-REG              OF BVVEB039
                                       TO BVVEW39I-S-HINCL-REG
           MOVE CUSUAR-INCL            OF BVVEB039
                                       TO BVVEW39I-S-CUSUAR-INCL
           MOVE HMANUT-REG             OF BVVEB039
                                       TO BVVEW39I-S-HMANUT-REG
           MOVE CUSUAR-MANUT           OF BVVEB039
                                       TO BVVEW39I-S-CUSUAR-MANUT.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR O PROCESSAMENTO.                         *
      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK
           .

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9200-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE
           MOVE SQLCA                  TO FRWKGDB2-SQLCA
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

