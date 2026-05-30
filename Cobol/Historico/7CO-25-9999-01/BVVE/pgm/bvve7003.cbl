      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID.         BVVE7003.
       AUTHOR.             MARTA ROMAO.

      *---------------------------------------------------------------*
      *     S T E F A N I N I - FABRICA DE SOFTWARE DE JAGUARIUNA     *
      *---------------------------------------------------------------*
      *                                                               *
      *     PROGRAMA.....: BVVE7003               - MODULO BATCH      *
      *     PROGRAMADORA.: MARTA ROMAO            - STEFANINI         *
      *     SUPERVISORA..: ANDREA MATTOS          - STEFANINI         *
      *     ACQ..........: RICARDO PINHO          - STEFANINI         *
      *     ANALISTA.....: ALESSANDRA GAMA        - DTS ALTRAN        *
      *     DATA.........: 28/10/2003             - 011 4193-4626 R306*
      *                                                               *
      *---------------------------------------------------------------*
      *     LINKAGE                                                   *
      *     -------                                                   *
      *     I#BVVE20   - AREA DE COMUNICACAO DOS PGMS CHAMADORES:     *
      *                  BVVE7915 - TRANS.COBOL SEM TELA ( ROTEADOR ) *
      *                  BVVE7920 - MODULO ACESSO MAINFRAME           *
      *                                                               *
      *                  CODIGOS DE RETORNOS:                         *
      *                  --------------------                         *
      *                  00 - ACESSO OK                               *
      *                  01 - ERRO DE CONSISTENCIA                    *
      *                  02 - ERRO LOGICO                             *
      *                  99 - ERRO DE ACESSO DB2                      *
      *                                                               *
      *     POL7100C -   AREA DE COMUNICACAO DA POOL7100              *
      *                                                               *
      *     POOL                                                      *
      *     ------                                                    *
      *     POOL1050 -   CONEXAO DB2                                  *
      *                                                               *
      *     TIPO DE BIND                                              *
      *     ( )X-OPCAO ANTIGA   ( )K-ONLY  ( )L-LIST   (X)A-ALL       *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *     OBJETIVO....: MODULO PARA TRATAR EXCLUSAO DE PROPOSTAS.   *
      *                                                               *
      *---------------------------------------------------------------*
           EJECT
      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.

           EJECT
      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

       01  FILLER                      PIC  X(035) VALUE
           'INICIO DA WORKING-STORAGE BVVE7003'.

      *---------------------------------------------------------------*
      *    AREAS AUXILIARES                                           *
      *---------------------------------------------------------------*
       77  WAUX-TEM-CPF            PIC X(03).

       01  WRK-WCONTD-PARM-TEXT.
           03  WRK-WCONTD-PARM-TEXT1   PIC X(03).
           03  FILLER                  PIC X(252).

       01  WRK-AREA-DADOS-TAB      PIC X(041).
       01  WRK-DADOS-CCNPJ  REDEFINES   WRK-AREA-DADOS-TAB.
7C2511     03  WRK-AUX-CCNPJ           PIC X(0009).
7C2511     03  WRK-AUX-CFLIAL          PIC X(0004).
           03  WRK-AUX-CCTRL           PIC 9(0002).
           03  WRK-AUX-DATULZ          PIC X(0026).
       01  WRK-DADOS-CPF    REDEFINES   WRK-AREA-DADOS-TAB.
           03  WRK-AUX-CPF             PIC 9(0009).
           03  WRK-AUX-DIGITO-CPF      PIC 9(0002).
           03  WRK-AUX-CCTRL-CPF       PIC X(0004).
           03  WRK-AUX-DATULZ-CPF      PIC X(0026).

      *         AREA AUXILIAR CHAVE PARA AS TABELAS
       01  WRK-DADOS-TAB           PIC X(041).
7C2511     03  WRK-CCNPJ           PIC X(0009).
7C2511     03  WRK-CFLIAL          PIC X(0004).
           03  WRK-CCTRL           PIC 9(0002).
           03  WRK-DATULZ          PIC X(0026).

      *---------------------------------------------------------------*
      *    AREA ERRO                                                  *
      *---------------------------------------------------------------*

-INC POL7100C

      *---------------------------------------------------------------*
      *    AREA DB2                                                   *
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB004
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB005
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB006
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB007
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB009

           EXEC SQL
                INCLUDE BVVEB010
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB011
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB015
           END-EXEC.

           EXEC SQL
                INCLUDE BVVEB016
           END-EXEC.

      *---------------------------------------------------------------*
       01   FILLER                     PIC  X(035) VALUE
           'FIM DA WORKING-STORAGE BVVE7003'.
      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

-INC I#BVVE20

       01  LKG-ERRO-AREA               PIC X(107).
       01  LKG-SQLCA                   PIC X(136).

      *---------------------------------------------------------------*
       PROCEDURE DIVISION              USING BVVE20-AREA-TRANSACAO
                                             LKG-ERRO-AREA
                                             LKG-SQLCA.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *---------------------------------------------------------------*

           PERFORM   00010-SITUACAO-PROCESSAR.
           PERFORM   10000-PROCEDIMENTOS-INICIAIS.

           PERFORM   20000-PROCEDIMENTO-PRINCIPAL.

           PERFORM   30000-FINALIZAR.

      *---------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       00010-SITUACAO-PROCESSAR          SECTION.
      *---------------------------------------------------------------*

             MOVE 'ON-LINE'  TO  IPARM-AFLIA-ESTBL  OF  BVVEB015.

             EXEC SQL
                SELECT
                     B.WCONTD_PARM
                INTO
                FROM   DB2PRD.PARM_AFLIA_ESTBL  A,
                       DB2PRD.VLR_PARM_AFLIA    B
                WHERE  A.IPARM_AFLIA_ESTBL = :BVVEB015.IPARM-AFLIA-ESTBL
                  AND  A.CPARM_AFLIA_ESTBL = B.CPARM_AFLIA_ESTBL
             END-EXEC.

             IF    ( SQLCODE  NOT = ZEROS   )  OR
                   ( SQLWARN0       =    'W'   )
                     MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                     MOVE 'VLR_PARM_AFLIA '        TO   ERR-DBD-TAB
                     MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                     MOVE SQLCODE                  TO   ERR-SQL-CODE
                     MOVE 0001                     TO   ERR-LOCAL
                     MOVE SPACES                   TO   ERR-SEGM
                     MOVE ERRO-AREA                TO   LKG-ERRO-AREA
                     MOVE SQLCA                    TO   LKG-SQLCA
                     PERFORM  40000-ROTINA-ERRO.

             MOVE  WCONTD-PARM OF BVVEB016   TO  WRK-WCONTD-PARM-TEXT.

                 MOVE   01                 TO   BVVE20-COD-RETORNO
                 MOVE  'APL'               TO   ERR-TIPO-ACESSO
                 MOVE  'BVVE7003'          TO   ERR-MODULO
                 MOVE  ' SISTEMA INDISPONIVEL PARA ATUALIZACOES. BATCH P
      -                'ROCESSANDO'        TO   BVVE20-MENSAGEM
                                                ERR-TEXTO
                 MOVE  ERRO-AREA           TO   LKG-ERRO-AREA
                 MOVE  SPACES              TO   LKG-SQLCA
                 GOBACK.

      *---------------------------------------------------------------*
       00010-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       10000-PROCEDIMENTOS-INICIAIS      SECTION.
      *---------------------------------------------------------------*

           MOVE BVVE20-AREA-DADOS      TO WRK-AREA-DADOS-TAB
                                          WRK-DADOS-TAB.
           MOVE SPACES                 TO BVVE20-AREA-RETORNO
                                          ERRO-AREA.

7C2511*    IF ( WRK-AUX-CCNPJ   NOT NUMERIC ) OR
7C2511*       ( WRK-AUX-CCNPJ   EQUAL ZEROS )
7C2511     IF ( WRK-AUX-CCNPJ   EQUAL SPACES ) OR
7C2511        ( WRK-AUX-CCNPJ   EQUAL LOW-VALUES )
                MOVE 01                 TO BVVE20-COD-RETORNO
                MOVE 'APL'              TO ERR-TIPO-ACESSO
                MOVE 'BVVE7003'         TO ERR-MODULO
                MOVE
                'ERRO DE CONSISTENCIA - CCNPJ IGUAL A BRANCOS OU ZEROS'
                                        TO ERR-TEXTO
                MOVE ERRO-AREA          TO LKG-ERRO-AREA
                MOVE SPACES             TO LKG-SQLCA
                GOBACK
           END-IF.

           IF  WRK-AUX-CCTRL-CPF       EQUAL  SPACES
               MOVE 'SIM'  TO  WAUX-TEM-CPF
               PERFORM 10010-CONSISTE-CPF
           ELSE
               PERFORM 10020-CONSISTE-CGC.

      *---------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       10010-CONSISTE-CPF                     SECTION.
      *---------------------------------------------------------------*

MARC  **** TIRADO A CONSISTENCIA PARA ACEITAR CPF COM DIGITO = ZEROS.
MARC  *
MARC  *    IF (WRK-AUX-DIGITO-CPF  EQUAL ZEROS)
MARC  *         MOVE 01                 TO BVVE20-COD-RETORNO
MARC  *         MOVE 'APL'              TO ERR-TIPO-ACESSO
MARC  *         MOVE 'BVVE7003'         TO ERR-MODULO
MARC  *         MOVE
MARC  *         'ERRO DE CONSISTENCIA - DIGITO IGUAL A ZEROS'
MARC  *                                 TO ERR-TEXTO
MARC  *         MOVE ERRO-AREA          TO LKG-ERRO-AREA
MARC  *         GOBACK
MARC  *    END-IF.

           MOVE  WRK-AUX-CPF            TO   WRK-CCNPJ.
           MOVE  WRK-AUX-DIGITO-CPF     TO   WRK-CCTRL
           MOVE  ZEROS                  TO   WRK-CFLIAL.
           MOVE  WRK-AUX-DATULZ-CPF     TO   WRK-DATULZ.

      *---------------------------------------------------------------*
       10010-99-FIM.                   EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       10020-CONSISTE-CGC                     SECTION.
      *---------------------------------------------------------------*

7C2511*    IF   WRK-AUX-CFLIAL   NOT NUMERIC
7C2511     IF ( WRK-AUX-CFLIAL   EQUAL SPACES ) OR
7C2511        ( WRK-AUX-CFLIAL   EQUAL LOW-VALUES )
                MOVE 01                 TO BVVE20-COD-RETORNO
                MOVE 'APL'              TO ERR-TIPO-ACESSO
                MOVE 'BVVE7003'         TO ERR-MODULO
                MOVE
                                        TO ERR-TEXTO
                MOVE ERRO-AREA          TO LKG-ERRO-AREA
                MOVE SPACES             TO LKG-SQLCA
                GOBACK
           END-IF.

           IF ( WRK-AUX-CCTRL   NOT NUMERIC )
                MOVE 01                 TO BVVE20-COD-RETORNO
                MOVE 'APL'              TO ERR-TIPO-ACESSO
                MOVE 'BVVE7003'         TO ERR-MODULO
                MOVE
                'DIGITO DE CONTROLE DO CNPJ DEVE SER NUMERICO'
                                        TO ERR-TEXTO
                MOVE ERRO-AREA          TO LKG-ERRO-AREA
                MOVE SPACES             TO LKG-SQLCA
                GOBACK
           END-IF.

           MOVE     WRK-AUX-CCNPJ       TO  WRK-CCNPJ.
           MOVE     WRK-AUX-CFLIAL      TO  WRK-CFLIAL.
           MOVE     WRK-AUX-DATULZ      TO  WRK-DATULZ.

      *---------------------------------------------------------------*
       10020-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       20000-PROCEDIMENTO-PRINCIPAL           SECTION.
      *---------------------------------------------------------------*

      * ******* ROTINA DE DELECAO ENDERECO
           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL   OF BVVEB003.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL OF BVVEB003.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF    OF BVVEB003.

           EXEC SQL
                 DELETE FROM DB2PRD.ENDER_ESTBL
                 WHERE CCNPJ_CPF_ESTBL   = :BVVEB003.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL = :BVVEB003.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF    = :BVVEB003.CCTRL-CNPJ-CPF

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                                             ERR-SQL-CODE
                MOVE 'ENDER_ESTBL       ' TO ERR-DBD-TAB
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0001'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

           IF   SQLCODE           EQUAL +100
                MOVE 02                   TO BVVE20-COD-RETORNO
                MOVE 'APL'                TO ERR-TIPO-ACESSO
                MOVE
             'CODIGO NAO ENCONTRADO NA BVVEB003 - EXCLUSAO NAO EFETUADA'
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SPACES               TO LKG-SQLCA
                GOBACK.

      * ******* ROTINA DE DELECAO FONE
           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL   OF BVVEB009.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL OF BVVEB009.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF    OF BVVEB009.

           EXEC SQL
                 DELETE FROM DB2PRD.FONE_ESTBL_AFLIA
                 WHERE CCNPJ_CPF_ESTBL   = :BVVEB009.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL = :BVVEB009.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF    = :BVVEB009.CCTRL-CNPJ-CPF
            END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                MOVE 'FONE_ESTBL_AFLIA  ' TO ERR-DBD-TAB
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0002'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

           IF   SQLCODE           EQUAL +100
                MOVE 02                   TO BVVE20-COD-RETORNO
                MOVE 'APL'                TO ERR-TIPO-ACESSO
                MOVE
             'CODIGO NAO ENCONTRADO NA BVVEB009 - EXCLUSAO NAO EFETUADA'
                                          TO ERR-TEXTO
                MOVE SPACES               TO LKG-ERRO-AREA
                MOVE SPACES               TO LKG-SQLCA
                GOBACK.

      * ******* ROTINA DE DELECAO PROPRIETARIO
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL OF BVVEB010.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF    OF BVVEB010.

           EXEC SQL
                 DELETE FROM DB2PRD.PPRIE_ESTBL_AFLIA
                 WHERE CCNPJ_CPF_ESTBL   = :BVVEB010.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL = :BVVEB010.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF    = :BVVEB010.CCTRL-CNPJ-CPF
            END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                                             ERR-SQL-CODE
                MOVE 'PPRIE_ESTBL_AFLIA ' TO ERR-DBD-TAB
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0003'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

           IF   SQLCODE           EQUAL +100
                MOVE 02                   TO BVVE20-COD-RETORNO
                MOVE 'APL'                TO ERR-TIPO-ACESSO
                MOVE
             'CODIGO NAO ENCONTRADO NA BVVEB010 - EXCLUSAO NAO EFETUADA'
                                          TO ERR-TEXTO
                MOVE SPACES               TO LKG-ERRO-AREA
                MOVE SPACES               TO LKG-SQLCA
                GOBACK.

      * ******* ROTINA DE DELECAO PROPOSTA
           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL    OF BVVEB011.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL  OF BVVEB011.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF     OF BVVEB011.
           MOVE WRK-DATULZ         TO DATULZ-PPSTA-AFLIA OF BVVEB011.

           EXEC SQL
                 WHERE CCNPJ_CPF_ESTBL    = :BVVEB011.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL  = :BVVEB011.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF     = :BVVEB011.CCTRL-CNPJ-CPF
                 AND   DATULZ_PPSTA_AFLIA = :BVVEB011.DATULZ-PPSTA-AFLIA
            END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                                             ERR-SQL-CODE
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'PPSTA_AFLIA_ESTBL ' TO ERR-DBD-TAB
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0004'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

                MOVE 02                   TO BVVE20-COD-RETORNO
                MOVE 'APL'                TO ERR-TIPO-ACESSO
                MOVE
                'EXCLUSAO NAO EFETUADA. MANUTENCAO EFETUADA POR OUTRO US
      -         'UARIO.'
                                          TO ERR-TEXTO
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SPACES               TO LKG-SQLCA
                GOBACK.

      * ******* ROTINA DE DELECAO ERROS ENDERECO
           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL    OF BVVEB004.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL  OF BVVEB004.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF     OF BVVEB004.

           EXEC SQL
                 DELETE FROM DB2PRD.ERRO_ENDER_ESTBL
                 WHERE CCNPJ_CPF_ESTBL    = :BVVEB004.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL  = :BVVEB004.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF     = :BVVEB004.CCTRL-CNPJ-CPF

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                                             ERR-SQL-CODE
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'ERRO_ENDER_ESTBL  ' TO ERR-DBD-TAB
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0005'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

      * ******* ROTINA DE DELECAO ERROS FONE
           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL    OF BVVEB005.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL  OF BVVEB005.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF     OF BVVEB005.

                 DELETE FROM DB2PRD.ERRO_FONE_ESTBL
                 WHERE CCNPJ_CPF_ESTBL    = :BVVEB005.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL  = :BVVEB005.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF     = :BVVEB005.CCTRL-CNPJ-CPF
            END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                                             ERR-SQL-CODE
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'ERRO_FONE_ESTBL   ' TO ERR-DBD-TAB
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0006'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL    OF BVVEB006.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL  OF BVVEB006.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF     OF BVVEB006.

           EXEC SQL
                 DELETE FROM DB2PRD.ERRO_PPRIE_ESTBL
                 WHERE CCNPJ_CPF_ESTBL    = :BVVEB006.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL  = :BVVEB006.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF     = :BVVEB006.CCTRL-CNPJ-CPF
            END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                                             ERR-SQL-CODE
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'ERRO_PPRIE_ESTBL  ' TO ERR-DBD-TAB
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0007'               TO ERR-LOCAL
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

      * ******* ROTINA DE DELECAO ERROS PROPOSTA
           MOVE WRK-CCNPJ          TO CCNPJ-CPF-ESTBL    OF BVVEB007.
           MOVE WRK-CFLIAL         TO CFLIAL-CNPJ-ESTBL  OF BVVEB007.
           MOVE WRK-CCTRL          TO CCTRL-CNPJ-CPF     OF BVVEB007.

           EXEC SQL
                 DELETE FROM DB2PRD.ERRO_PPSTA_AFLIA
                 WHERE CCNPJ_CPF_ESTBL    = :BVVEB007.CCNPJ-CPF-ESTBL
                 AND   CFLIAL_CNPJ_ESTBL  = :BVVEB007.CFLIAL-CNPJ-ESTBL
                 AND   CCTRL_CNPJ_CPF     = :BVVEB007.CCTRL-CNPJ-CPF
            END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100 ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 99                   TO BVVE20-COD-RETORNO
                MOVE SQLCODE              TO BVVE20-COD-SQLCODE
                MOVE 'DB2'                TO ERR-TIPO-ACESSO
                MOVE 'ERRO_PPSTA_AFLIA  ' TO ERR-DBD-TAB
                MOVE 'DELETE'             TO ERR-FUN-COMANDO
                MOVE '0008'               TO ERR-LOCAL
                MOVE SPACES               TO ERR-SEGM
                MOVE ERRO-AREA            TO LKG-ERRO-AREA
                MOVE SQLCA                TO LKG-SQLCA
                GOBACK.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR  SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS              TO BVVE20-COD-RETORNO
                                      BVVE20-COD-SQLCODE.

           MOVE 'EXCLUSAO EFETUADA COM SUCESSO'
           GOBACK.
      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40000-ROTINA-ERRO               SECTION.
      *----------------------------------------------------------------*

           MOVE     99                 TO          BVVE20-COD-RETORNO.
           MOVE     'BVVE7003'         TO          ERR-PGM.

           GOBACK.

      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
