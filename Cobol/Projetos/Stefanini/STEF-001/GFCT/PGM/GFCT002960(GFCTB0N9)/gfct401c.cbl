      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     GFCT401C.
       AUTHOR.                         STEFANINI.
      *----------------------------------------------------------------*
      *    PROGRAMA....: GFCT401C.                                     *
      *    PROGRAMADOR.: STEFANINI                                     *
      *    ANALISTA....: STEFANINI                                     *
      *    DATA........: 07/2025                                       *
      *----------------------------------------------------------------*
      *    OBJETIVO....: CONSULTAR LISTA DE CLIENTES SEM CESTA         *
      *                  CLONE PROGRAMA GFCT400s                       *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TCCLI_SEM_CESTA                               GFCTB0N9      *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    GFCTW100 - BOOK DE CONTROLE                                 *
      *    GFCTW11C - BOOK DE ENTRADA                                  *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKHE COMMAREA FRWK1999 (AREA COMUM ERROS)               *
      *    I#FRWKDB COMMAREA FRWK1999 (LOG DE ERROS DB2 )              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
      *================================================================*
      *
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
           'INICIO DA WORKING GFCT401C'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA INDEXADORES'.
      *----------------------------------------------------------------*
       77  WRK-IND-I                   PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
       77  WRK-CONTADOR                PIC S9(011) COMP-3  VALUE ZEROS.
       77  WRK-NUMBER-ROW              PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-AUXILIARES.
           05  WRK-FRWK1999            PIC X(008)      VALUE 'FRWK1999'.
           05  WRK-PROGRAMA            PIC X(008)      VALUE 'GFCT401C'.
           05  WRK-TABELA              PIC X(032)      VALUE
               'DB2PRD.TCLI_SEM_CESTA'.
           05  WRK-FIM-CURSOR          PIC X(001)      VALUE 'N'.

       01  WRK-NULIDADE.
           05 WRK-CID-RMAL-NULL        PIC S9(004) COMP  VALUE +0.
           05 WRK-CDDI-NULL            PIC S9(004) COMP  VALUE +0.
           05 WRK-CDDD-NULL            PIC S9(004) COMP  VALUE +0.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
             10 WRK-CHAR-INFO-ERRO     PIC X(01) OCCURS 0 TO 222   TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA DE TRATAMENTO DE ERRO DB2 (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-DB2.
           COPY 'I#FRWKDB'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0N9
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE
           'AREA PARA CURSORES'.
      *----------------------------------------------------------------*

           EXEC SQL
                DECLARE CSR01-GFCTB0N9 INSENSITIVE SCROLL CURSOR FOR
                SELECT  CAG_BCRIA
                       ,CFUNC_BDSCO
                       ,CSERVC_TARIF
                       ,CINDCD_VNCLO_FL
                       ,VRENTB
                       ,CCTA_BCRIA
                       ,CCPF_CNPJ
                       ,CCTRL_CPF_CNPJ
ST25X6                 ,CCPF_CNPJ_ST
ST25X6                 ,CCTRL_CPF_CNPJ_ST
                       ,IPSSOA_COPLT
                       ,CPSSOA
                       ,CDDI
                       ,CDDD
                       ,NLIN_TFONI
                       ,CID_RMAL
                       ,CINDCD_REST_CNTAT
                       ,DABERT_CTA
                       ,VINVES
                FROM  DB2PRD.TCLI_SEM_CESTA
               WHERE CFUNC_BDSCO = :GFCTB0N9.CFUNC-BDSCO
                 AND CAG_BCRIA   = :GFCTB0N9.CAG-BCRIA
               ORDER BY  CFUNC_BDSCO
                        ,CAG_BCRIA
                        ,CSERVC_TARIF
                        ,CINDCD_VNCLO_FL DESC
                        ,VRENTB
                        ,CCTA_BCRIA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)  VALUE
           'FIM DA WORKING GFCT401C'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY GFCTW100.
           COPY GFCTW11C.

      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

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
                      GFCTW100-BLOCO-RETORNO.

           PERFORM 1100-CONSISTIR-DADOS-ENTRADA.

           PERFORM 1200-VERIF-PAGINACAO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTIR OS DADOS DE ENTRADA.                  *
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTW11C-E-CFUNC-BDSCO  NOT NUMERIC
               MOVE 08                 TO GFCTW100-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0001'             TO GFCTW100-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'GFCT0002'         TO GFCTW100-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  (GFCTW11C-E-CAG-BCRIA    NOT NUMERIC OR
                GFCTW11C-E-CAG-BCRIA    EQUAL ZEROS)
               MOVE 08                 TO GFCTW100-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0001'             TO GFCTW100-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'GFCT0002'         TO GFCTW100-COD-MENSAGEM
                                       OF DFHCOMMAREA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-VERIF-PAGINACAO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTW11C-E-CFUNC-BDSCO
                                       TO CFUNC-BDSCO OF GFCTB0N9.
           MOVE GFCTW11C-E-CAG-BCRIA
                                       TO CAG-BCRIA   OF GFCTB0N9.

           EVALUATE TRUE
               WHEN GFCTW11C-P-INICIAL
               WHEN GFCTW11C-P-PRIMEIRA
                   MOVE 1              TO WRK-NUMBER-ROW

               WHEN GFCTW11C-P-SEGUINTE
                   MOVE GFCTW11C-F-NUMBER-ROW
                                       TO WRK-NUMBER-ROW
                   ADD 1               TO WRK-NUMBER-ROW

               WHEN GFCTW11C-P-ANTERIOR
                   MOVE GFCTW11C-I-NUMBER-ROW
                                       TO WRK-NUMBER-ROW
                   SUBTRACT 1          FROM WRK-NUMBER-ROW

               WHEN GFCTW11C-P-ULTIMA
                   PERFORM 1210-CONTADOR-LINHAS
                   MOVE WRK-CONTADOR   TO WRK-NUMBER-ROW

           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       1210-CONTADOR-LINHAS            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
                 INTO :WRK-CONTADOR
                 FROM  DB2PRD.TCLI_SEM_CESTA
                WHERE CFUNC_BDSCO = :GFCTB0N9.CFUNC-BDSCO
                  AND CAG_BCRIA   = :GFCTB0N9.CAG-BCRIA
           END-EXEC.

           IF ((SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W'))
               SET DB2-SELECT          TO TRUE
               MOVE '0080'             TO GFCTW100-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE WRK-TABELA         TO FRWKGDB2-NOME-TABELA
               MOVE '1210-CONTADOR-LINHAS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ABRIR-CURSOR.

           PERFORM UNTIL WRK-IND-I         NOT LESS 50 OR
               WRK-FIM-CURSOR          EQUAL 'S'
               PERFORM 2300-FORMATAR-SAIDA
               PERFORM 2200-LER-TABELA-GFCTB0N9
           END-PERFORM.

           PERFORM 2400-FECHAR-CURSOR.

      *--> FORMATAR BLOCO-RETORNO COM SUCESSO
           MOVE WRK-IND-I              TO GFCTW11C-S-QTD-LISTA-OCORR

           IF GFCTW11C-S-QTD-LISTA-OCORR GREATER ZEROS
              IF WRK-FIM-CURSOR        EQUAL 'S'
                 MOVE ZEROS            TO GFCTW100-COD-RETORNO
              ELSE
                 MOVE 01               TO GFCTW100-COD-RETORNO
              END-IF
              MOVE '0000'              TO GFCTW100-COD-ERRO
      *--------> CONSULTA EFETUADA COM SUCESSO
              MOVE 'GFCT0005'          TO GFCTW100-COD-MENSAGEM
           ELSE
               IF  GFCTW11C-P-SEGUINTE OR
                   GFCTW11C-P-ANTERIOR OR
                   GFCTW11C-P-ULTIMA
                   MOVE ZEROS          TO GFCTW100-COD-RETORNO
               ELSE
                   MOVE 08             TO GFCTW100-COD-RETORNO
               END-IF
               MOVE '0000'             TO GFCTW100-COD-ERRO
      *--------> REGISTRO NAO ENCONTRADO PARA A CONSULTA
               MOVE 'GFCT0012'         TO GFCTW100-COD-MENSAGEM
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR FORMATAR A DCLGEN PARA ACESSAR A TABELA *
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0N9
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-OPEN           TO TRUE
               MOVE WRK-TABELA         TO FRWKGDB2-NOME-TABELA
               MOVE '2100-ABRIR-CURSOR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0001'             TO GFCTW100-COD-ERRO
               MOVE 'GFCT0003'         TO GFCTW100-COD-MENSAGEM
               PERFORM 9000-ERRO-DB2
           END-IF.

           EXEC SQL
               FETCH ABSOLUTE :WRK-NUMBER-ROW
                     CSR01-GFCTB0N9
                INTO :GFCTB0N9.CAG-BCRIA
                    ,:GFCTB0N9.CFUNC-BDSCO
                    ,:GFCTB0N9.CSERVC-TARIF
                    ,:GFCTB0N9.CINDCD-VNCLO-FL
                    ,:GFCTB0N9.VRENTB
                    ,:GFCTB0N9.CCTA-BCRIA
                    ,:GFCTB0N9.CCPF-CNPJ
                    ,:GFCTB0N9.CCTRL-CPF-CNPJ
ST25X6              ,:GFCTB0N9.CCPF-CNPJ-ST
ST25X6              ,:GFCTB0N9.CCTRL-CPF-CNPJ-ST
                    ,:GFCTB0N9.IPSSOA-COPLT
                    ,:GFCTB0N9.CPSSOA
                    ,:GFCTB0N9.CDDI
                       :WRK-CDDI-NULL
                    ,:GFCTB0N9.CDDD
                       :WRK-CDDD-NULL
                    ,:GFCTB0N9.NLIN-TFONI
                    ,:GFCTB0N9.CID-RMAL
                       :WRK-CID-RMAL-NULL
                    ,:GFCTB0N9.CINDCD-REST-CNTAT
                    ,:GFCTB0N9.DABERT-CTA
                    ,:GFCTB0N9.VINVES
           END-EXEC.

           IF ((SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W'))
               SET DB2-OPEN            TO TRUE
               MOVE '0080'             TO GFCTW100-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE WRK-TABELA         TO FRWKGDB2-NOME-TABELA
               MOVE '2100-ABRIR-TAB-GOTFB064'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

           IF WRK-CID-RMAL-NULL LESS ZEROS
              INITIALIZE CID-RMAL OF GFCTB0N9
           END-IF.

           IF WRK-CDDI-NULL     LESS ZEROS
              INITIALIZE CDDI  OF GFCTB0N9
           END-IF.

           IF WRK-CDDD-NULL     LESS ZEROS
              INITIALIZE CDDD  OF GFCTB0N9
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR ACESSAR A TABELA GFCTB0N9               *
      *----------------------------------------------------------------*
       2200-LER-TABELA-GFCTB0N9        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTW11C-P-INICIAL OR
               GFCTW11C-P-PRIMEIRA OR
               GFCTW11C-P-SEGUINTE
               EXEC SQL
                   FETCH RELATIVE +1 CSR01-GFCTB0N9
                INTO :GFCTB0N9.CAG-BCRIA
                    ,:GFCTB0N9.CFUNC-BDSCO
                    ,:GFCTB0N9.CSERVC-TARIF
                    ,:GFCTB0N9.CINDCD-VNCLO-FL
                    ,:GFCTB0N9.VRENTB
                    ,:GFCTB0N9.CCTA-BCRIA
                    ,:GFCTB0N9.CCPF-CNPJ
                    ,:GFCTB0N9.CCTRL-CPF-CNPJ
ST25X6              ,:GFCTB0N9.CCPF-CNPJ-ST
ST25X6              ,:GFCTB0N9.CCTRL-CPF-CNPJ-ST
                    ,:GFCTB0N9.IPSSOA-COPLT
                    ,:GFCTB0N9.CPSSOA
                    ,:GFCTB0N9.CDDI
                       :WRK-CDDI-NULL
                    ,:GFCTB0N9.CDDD
                       :WRK-CDDD-NULL
                    ,:GFCTB0N9.NLIN-TFONI
                    ,:GFCTB0N9.CID-RMAL
                       :WRK-CID-RMAL-NULL
                    ,:GFCTB0N9.CINDCD-REST-CNTAT
                    ,:GFCTB0N9.DABERT-CTA
                    ,:GFCTB0N9.VINVES
               END-EXEC
           ELSE
               EXEC SQL
                   FETCH RELATIVE -1 CSR01-GFCTB0N9
                INTO :GFCTB0N9.CAG-BCRIA
                    ,:GFCTB0N9.CFUNC-BDSCO
                    ,:GFCTB0N9.CSERVC-TARIF
                    ,:GFCTB0N9.CINDCD-VNCLO-FL
                    ,:GFCTB0N9.VRENTB
                    ,:GFCTB0N9.CCTA-BCRIA
                    ,:GFCTB0N9.CCPF-CNPJ
                    ,:GFCTB0N9.CCTRL-CPF-CNPJ
ST25X6              ,:GFCTB0N9.CCPF-CNPJ-ST
ST25X6              ,:GFCTB0N9.CCTRL-CPF-CNPJ-ST
                    ,:GFCTB0N9.IPSSOA-COPLT
                    ,:GFCTB0N9.CPSSOA
                    ,:GFCTB0N9.CDDI
                       :WRK-CDDI-NULL
                    ,:GFCTB0N9.CDDD
                       :WRK-CDDD-NULL
                    ,:GFCTB0N9.NLIN-TFONI
                    ,:GFCTB0N9.CID-RMAL
                       :WRK-CID-RMAL-NULL
                    ,:GFCTB0N9.CINDCD-REST-CNTAT
                    ,:GFCTB0N9.DABERT-CTA
                    ,:GFCTB0N9.VINVES
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-FETCH          TO TRUE
               MOVE WRK-TABELA         TO FRWKGDB2-NOME-TABELA
               MOVE '2200-LER-TABELA-GFCTB0N9'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0001'             TO GFCTW100-COD-ERRO
               MOVE 'GFCT0003'         TO GFCTW100-COD-MENSAGEM
               PERFORM 9000-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

           IF WRK-CID-RMAL-NULL LESS ZEROS
              INITIALIZE CID-RMAL OF GFCTB0N9
           END-IF.

           IF WRK-CDDI-NULL     LESS ZEROS
              INITIALIZE CDDI  OF GFCTB0N9
           END-IF.

           IF WRK-CDDD-NULL     LESS ZEROS
              INITIALIZE CDDD  OF GFCTB0N9
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FORMATAR SAIDA.                                    *
      *----------------------------------------------------------------*
       2300-FORMATAR-SAIDA             SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO WRK-IND-I

           MOVE CFUNC-BDSCO            OF GFCTB0N9
                                       TO GFCTW11C-S-CFUNC-BDSCO
                                          (WRK-IND-I)
           MOVE CSERVC-TARIF           OF GFCTB0N9
                                       TO GFCTW11C-S-CSERVC-TARIF
                                          (WRK-IND-I)
           MOVE CINDCD-VNCLO-FL        OF GFCTB0N9
                                       TO GFCTW11C-S-CINDCD-VINCLO-FL
                                          (WRK-IND-I).
           IF VRENTB OF GFCTB0N9       IS NEGATIVE
              MOVE '-'                 TO GFCTW11C-S-SINAL-VRENTB
                                          (WRK-IND-I)
           ELSE
              MOVE '+'                 TO GFCTW11C-S-SINAL-VRENTB
                                          (WRK-IND-I)
           END-IF.

           MOVE VRENTB                 OF GFCTB0N9
                                       TO GFCTW11C-S-VRENTB
                                          (WRK-IND-I)

           IF VINVES OF GFCTB0N9       IS NEGATIVE
              MOVE '-'                 TO GFCTW11C-S-SINAL-VINVES
                                          (WRK-IND-I)
           ELSE
              MOVE '+'                 TO GFCTW11C-S-SINAL-VINVES
                                          (WRK-IND-I)
           END-IF.

           MOVE VINVES                 OF GFCTB0N9
                                       TO GFCTW11C-S-VINVES
                                          (WRK-IND-I)

           MOVE CCTA-BCRIA             OF GFCTB0N9
                                       TO GFCTW11C-S-CCTA-BCRIA
                                          (WRK-IND-I)
ST25X6*    MOVE CCPF-CNPJ              OF GFCTB0N9
ST25X6*                                TO GFCTW11C-S-CCPF-CNPJ
ST25X6*                                   (WRK-IND-I)
ST25X6     MOVE CCPF-CNPJ-ST           OF GFCTB0N9
ST25X6                                 TO GFCTW11C-S-CCPF-CNPJ
ST25X6                                    (WRK-IND-I)
ST25X6*    MOVE CCTRL-CPF-CNPJ         OF GFCTB0N9
ST25X6*                                TO GFCTW11C-S-CCTRL-CPF-CNPJ
ST25X6*                                   (WRK-IND-I)
ST25X6     MOVE CCTRL-CPF-CNPJ-ST      OF GFCTB0N9
ST25X6                                 TO GFCTW11C-S-CCTRL-CPF-CNPJ
ST25X6                                    (WRK-IND-I)
           MOVE IPSSOA-COPLT           OF GFCTB0N9
                                       TO GFCTW11C-S-IPSSOA-COPLT
                                          (WRK-IND-I)
           MOVE CPSSOA                 OF GFCTB0N9
                                       TO GFCTW11C-S-CPSSOA
                                          (WRK-IND-I)
           MOVE CDDI                   OF GFCTB0N9
                                       TO GFCTW11C-S-CDDI
                                          (WRK-IND-I)
           MOVE CDDD                   OF GFCTB0N9
                                       TO GFCTW11C-S-CDDD
                                          (WRK-IND-I)
           MOVE NLIN-TFONI             OF GFCTB0N9
                                       TO GFCTW11C-S-NLIN-TFONI
                                          (WRK-IND-I)
           MOVE CID-RMAL               OF GFCTB0N9
                                       TO GFCTW11C-S-CID-RMAL-BIP-PAGER
                                          (WRK-IND-I)
           MOVE CINDCD-REST-CNTAT      OF GFCTB0N9
                                       TO GFCTW11C-S-CINDCD-REST-CNTAT
                                          (WRK-IND-I)
           MOVE DABERT-CTA             OF GFCTB0N9
                                       TO GFCTW11C-S-DABERT-CTA
                                          (WRK-IND-I).

           IF  WRK-IND-I                   EQUAL 1
               IF  GFCTW11C-P-PRIMEIRA OR
                   GFCTW11C-P-INICIAL  OR
                   GFCTW11C-P-SEGUINTE
                   MOVE WRK-NUMBER-ROW TO GFCTW11C-I-NUMBER-ROW
               ELSE
                   MOVE WRK-NUMBER-ROW TO GFCTW11C-F-NUMBER-ROW
               END-IF
           ELSE
               IF  GFCTW11C-P-PRIMEIRA OR
                   GFCTW11C-P-INICIAL  OR
                   GFCTW11C-P-SEGUINTE
                   ADD 1               TO WRK-NUMBER-ROW
                   MOVE WRK-NUMBER-ROW TO GFCTW11C-F-NUMBER-ROW
               ELSE
                   SUBTRACT 1          FROM WRK-NUMBER-ROW
                   MOVE WRK-NUMBER-ROW TO GFCTW11C-I-NUMBER-ROW
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FECHAMENTO DO CURSOR.                              *
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0N9
           END-EXEC.

           IF  SQLCODE                 NOT EQUAL ZEROS OR
               SQLWARN0                EQUAL 'W'
               SET  DB2-CLOSE          TO TRUE
               MOVE WRK-TABELA         TO FRWKGDB2-NOME-TABELA
               MOVE '2400-FECHAR-CURSOR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0001'             TO GFCTW100-COD-ERRO
               MOVE 'GFCT0003'         TO GFCTW100-COD-MENSAGEM
               PERFORM 9000-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR O PROCESSAMENTO.                         *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-DB2               TO TRUE.
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.
           MOVE SQLCA                  TO FRWKGDB2-SQLCA.
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE.
           MOVE 12                     TO GFCTW100-COD-RETORNO.
           MOVE WRK-COPY-DB2           TO WRK-BLOCO-INFO-ERRO
                                          (1:222).

           PERFORM 9999-API-ERROS.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO CHAMADA AO FRWK1999.                 *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
           END-EXEC.

           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
               CONTINUE
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
