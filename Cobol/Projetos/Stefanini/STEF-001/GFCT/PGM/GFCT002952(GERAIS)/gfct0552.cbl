      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0552.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0552                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   VALERIA TORQUATO        - PROCWORK - GP.50  *
      *    DATA........:   03/06/2009                                  *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      CONSULTA DETALHADA DA AUTORIZACAO DE EMISSAO DE EXTRATO   *
      *      ANUAL DE TARIFAS VIA CORREIO.                             *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.TAUTRZ_ENVIO_AGPTO              GFCTB0L4          *
      *       DB2PRD.TTPO_SGMTO_GSTAO                GFCTB0H5          *
      *       DB2PRD.V01CLIENTE_AGENCIA              CLIEV004          *
      *                                                                *
      *   MODULOS CHAMADOS:                                            *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    GFCT5522 - VERIFICACAO DE DISPONIBILIDADE ON-LINE           *
      *    GFCT5090 - OBTER DESCRICAO DO CORRESPONDENTE BANCARIO       *
      *    SARH8000 - MODULO PARA PERMITIR AOS SISTEMAS LEGADOS O ACES-*
      *               SO AS INFORMACOES DE FUNCIONARIOS.               *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7548                                     *
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
           '*  INICIO DA WORKING GFCT0552  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
              10 FILLER                PIC  X(006).
              10 WRK-AUX-9-X.
                 15 WRK-AUX-9-3        PIC  9(003).

       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.

       01  WRK-HINCL.
           03  WRK-DATA.
               05  WRK-ANO             PIC 9(04)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-MES             PIC 9(02)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-DIA             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE SPACES.
           03  WRK-HORA.
               05  WRK-HH              PIC 9(02)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-MM              PIC 9(02)           VALUE ZEROS.
               05  FILLER              PIC X(01)           VALUE SPACES.
               05  WRK-SS              PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(07)           VALUE SPACES.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-MES-AUX             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-ANO-AUX             PIC 9(04)           VALUE ZEROS.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MSG05-MODULO        PIC X(08)           VALUE SPACES.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(041)         VALUE SPACES.

       01  WRK-MSG002                  PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM SARH8000'.
      *----------------------------------------------------------------*

       01 WRK-ENTRADA.
          05 WRK-PESQUISA-ENT              PIC 9(009)      VALUE ZEROS.
          05 WRK-TAMANHO-ENT               PIC 9(004)      VALUE ZEROS.
          05 WRK-SOLICITA-ENT.
            10 FILLER                      OCCURS 255 TIMES.
              15 WRK-CAMPO-ENT             PIC 9(003)      VALUE ZEROS.

       01 WRK-SAIDA.
          05 WRK-COD-RETORNO-SAI           PIC 9(002)      VALUE ZEROS.
          05 WRK-TAMANHO-SAI               PIC 9(004)      VALUE ZEROS.
          05 WRK-REGISTRO-SAI.
            10 FILLER                      OCCURS 0 TO 1000 TIMES
                                           DEPENDING ON WRK-TAMANHO-SAI.
              15 WRK-POSICAO-SAI           PIC X(001)      VALUE SPACES.

       01  WRK-AREA-RET.
           05 WRK-NOME-RET                 PIC  X(040)     VALUE SPACES.
           05 FILLER                       PIC  X(040)     VALUE SPACES.

       01  WRK-ERRO-AREA                   PIC  X(107)     VALUE SPACES.
       01  WRK-SQLCA                       PIC  X(136)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5523'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.

       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'AREA DE COMUNICACAO COM GFCT5522'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.

       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5090   *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGB.

       COPY I#GFCTGC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0L4
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0H5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT0552   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAGS.
       COPY GFCTWAGT.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING WAGS-ENTRADA
                                             WAGT-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *  ROTINA PRINCIPAL DO PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  INICIALIZACAO DO PROGRAMA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAGT-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                     WAGT-SAIDA
                                          GFCT0M-AREA-ERROS
                                          SQLCA.

           MOVE 1010                   TO WAGT-LL
           MOVE WAGS-TRANSACAO         TO WAGT-TRANSACAO
           MOVE WAGS-FUNCAO            TO WAGT-FUNCAO
           MOVE WAGS-FUNC-BDSCO        TO WAGT-FUNC-BDSCO
           MOVE ZEROS                  TO WAGT-ERRO
                                          WAGT-COD-SQL-ERRO
                                          WAGT-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.
           MOVE 'N'                    TO WAGT-FIM

           PERFORM 1100-OBTER-DESC-MSG

           PERFORM 1200-CONSISTIR-CAMPOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER DESCRICAO DA MENSAGEM                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     GFCTG3-SAIDA

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAGS-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAGS-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAGS-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE      NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAGT-DESC-MSG-ERRO
               MOVE 9                  TO WAGT-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0552'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAGT-DESC-MSG-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAGT-COD-SQL-ERRO
                   MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAGT-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0552'         TO GFCT0M-TRANSACAO
               MOVE GFCTG3-ERRO        TO WAGT-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE GFCTG3-ERRO        TO WAGT-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAGT-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAGT-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAGT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTG3-DESC-MSG        TO WAGT-DESC-MSG-ERRO
                                         (1: LENGTH OF GFCTG3-DESC-MSG)
                                          GFCT0M-TEXTO
                                         (1: LENGTH OF GFCTG3-DESC-MSG).

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER CONSISTENCIA DE CAMPOS                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-VERIFICAR-ONLINE.

           IF (WAGS-TRANSACAO       EQUAL SPACES  OR LOW-VALUES) OR
              (WAGS-FUNCAO          EQUAL SPACES  OR LOW-VALUES) OR
              (WAGS-FUNC-BDSCO      EQUAL SPACES  OR LOW-VALUES) OR
              (WAGS-CAGPTO            NOT NUMERIC              ) OR
              (WAGS-CAGPTO          EQUAL ZEROS                ) OR
              (WAGS-HINCL           EQUAL SPACES  OR LOW-VALUES)
               MOVE 1                  TO WAGT-ERRO
               MOVE ZEROS              TO WAGT-COD-SQL-ERRO
               MOVE 0001               TO WAGT-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAGT-FIM
               PERFORM 1100-OBTER-DESC-MSG
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  VERIFICA DISPONIBILIDADE ONLINE                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +101                   TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAGS-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAGS-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAGS-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO RETURN-CODE
               MOVE SPACES             TO WAGT-DESC-MSG-ERRO
               MOVE 9                  TO WAGT-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             NOT EQUAL ZEROS
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE SPACES         TO WAGT-DESC-MSG-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAGT-COD-SQL-ERRO
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WAGT-DESC-MSG-ERRO
               END-IF

               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WAGT-ERRO

               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAGT-ERRO
               MOVE SPACES             TO WAGT-DESC-MSG-ERRO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0198'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO PRINCIPAL                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-GFCTB0L4

           PERFORM 2200-MOVER-CAMPOS

           IF  WAGS-CAGPTO          EQUAL 4
               PERFORM 2300-TRATAR-CLIENTE
           ELSE
             IF  WAGS-CAGPTO        EQUAL 9
                 PERFORM 2400-TRATAR-SEGMTO
             ELSE
                 PERFORM 2500-TRATAR-POSTO
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DADOS DO AGRUPAMENTO SELECIONADO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ACESSAR-GFCTB0L4           SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L4
           MOVE WAGS-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L4
           MOVE WAGS-HINCL             TO HINCL-REG-SIST     OF GFCTB0L4

           EXEC SQL
               SELECT  CFUNC_INCL_SIST,
                       RJUSTF_INCL_AUTRZ,
                       HEXCL_REG_SIST,
                       CFUNC_EXCL_SIST,
                       RJUSTF_EXCL_AUTRZ
               INTO    :GFCTB0L4.CFUNC-INCL-SIST,
                       :GFCTB0L4.RJUSTF-INCL-AUTRZ,
                       :GFCTB0L4.HEXCL-REG-SIST,
                       :GFCTB0L4.CFUNC-EXCL-SIST,
                       :GFCTB0L4.RJUSTF-EXCL-AUTRZ
               FROM    DB2PRD.TAUTRZ_ENVIO_AGPTO
               WHERE   CAGPTO_CTA         = :GFCTB0L4.CAGPTO-CTA
                 AND   CINDCD_AUTRZ_EMIS  = :GFCTB0L4.CINDCD-AUTRZ-EMIS
                 AND   HINCL_REG_SIST     = :GFCTB0L4.HINCL-REG-SIST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TAUTRZ_ENVIO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGT-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAGT-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0L4'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  MOVER CAMPOS PARA ERRO DB2                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT0552'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO WAGT-ERRO.

           PERFORM 1100-OBTER-DESC-MSG.

           MOVE SPACES                 TO WAGT-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG   ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO WAGT-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  MOVER CAMPOS EXTRAIDOS DA TABELA DE AGRUPAMENTOS              *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

           MOVE WAGS-CAGPTO            TO WAGT-CAGPTO
           MOVE WAGS-HINCL             TO WAGT-HINCL
                                          WRK-HINCL
           MOVE WRK-DIA                TO WRK-DIA-AUX
           MOVE WRK-MES                TO WRK-MES-AUX
           MOVE WRK-ANO                TO WRK-ANO-AUX
           MOVE WRK-DATA-AUX           TO WAGT-DATA-INCL
           MOVE WRK-HORA               TO WAGT-HORA-INCL

           MOVE RJUSTF-INCL-AUTRZ-TEXT OF GFCTB0L4 (1:254)
                                       TO WAGT-JUSTIF-INCL

           MOVE CFUNC-INCL-SIST        OF GFCTB0L4
                                       TO WRK-AUX-9-S
           MOVE WRK-AUX-9              TO WAGT-CFUNC-INCL
                                          WRK-PESQUISA-ENT

           PERFORM 2210-OBTER-NM-FUNC

           MOVE WRK-NOME-RET (1:30)    TO WAGT-NFUNC-INCL

           IF  CFUNC-EXCL-SIST OF GFCTB0L4 NOT EQUAL ZEROS
               MOVE HEXCL-REG-SIST     OF GFCTB0L4
                                       TO WRK-HINCL
               MOVE WRK-DIA            TO WRK-DIA-AUX
               MOVE WRK-MES            TO WRK-MES-AUX
               MOVE WRK-ANO            TO WRK-ANO-AUX
               MOVE WRK-DATA-AUX       TO WAGT-DATA-EXCL
               MOVE WRK-HORA           TO WAGT-HORA-EXCL

               MOVE RJUSTF-EXCL-AUTRZ-TEXT OF GFCTB0L4 (1:254)
                                       TO WAGT-JUSTIF-EXCL

               MOVE CFUNC-EXCL-SIST    OF GFCTB0L4
                                       TO WRK-AUX-9-S
               MOVE WRK-AUX-9          TO WAGT-CFUNC-EXCL
                                          WRK-PESQUISA-ENT

               PERFORM 2210-OBTER-NM-FUNC

               MOVE WRK-NOME-RET(1:30) TO WAGT-NFUNC-EXCL
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER NOME DO FUNCIONARIO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-OBTER-NM-FUNC              SECTION.
      *----------------------------------------------------------------*

           MOVE 0086                   TO WRK-TAMANHO-ENT.
           MOVE 002                    TO WRK-CAMPO-ENT(1).
           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO          USING WRK-ENTRADA
                                          WRK-SAIDA
                                          WRK-ERRO-AREA
                                          WRK-SQLCA.

           IF  WRK-COD-RETORNO-SAI  EQUAL 02
               MOVE 'FUNCIONARIO NAO ENCONTRADO '
                                       TO WRK-NOME-RET
               GO                      TO 2210-99-FIM
           END-IF.

           IF  WRK-COD-RETORNO-SAI  EQUAL ZEROS
               MOVE WRK-REGISTRO-SAI(1:80)
                                       TO WRK-AREA-RET
               GO                      TO 2210-99-FIM
           END-IF.

           IF  WRK-COD-RETORNO-SAI  EQUAL 10 OR 12
               MOVE ALL '*'            TO WRK-NOME-RET
           ELSE
               MOVE SPACES             TO WRK-NOME-RET
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR CLIENTE SELECIONADO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-TRATAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           MOVE WAGS-NUMERO            TO WAGT-NUMERO
                                          CCGC-CPF          OF CLIEV004
           MOVE WAGS-FILIAL            TO WAGT-FILIAL
                                          CFLIAL-CGC        OF CLIEV004
           MOVE WAGS-CONTRL            TO WAGT-CONTRL
                                          CCTRL-CPF-CGC     OF CLIEV004

           PERFORM 2310-ACESSAR-CLIEV004.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTER DESCRICAO DO CLIENTE                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT  IPSSOA_COPLT
               INTO    :CLIEV004.IPSSOA-COPLT
               FROM    DB2PRD.V01CLIENTE_AGENCIA
               WHERE  CCGC_CPF           = :CLIEV004.CCGC-CPF
                 AND  CFLIAL_CGC         = :CLIEV004.CFLIAL-CGC
                 AND  CCTRL_CPF_CGC      = :CLIEV004.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'V01CLIENTE_AGENCIA'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGT-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAGT-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'CLIEV004'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE IPSSOA-COPLT           OF CLIEV004 (1:30)
                                       TO WAGT-NM-CLIE.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR CURSOR CSR02 OBTER SEGMENTO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-TRATAR-SEGMTO              SECTION.
      *----------------------------------------------------------------*

           MOVE WAGS-CSEGMTO           TO WAGT-CSEGMTO
                                          CSGMTO-GSTAO-TARIF OF GFCTB0H5

           PERFORM 2410-OBTER-SEGMTO.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM NOME DO SEGMENTO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-OBTER-SEGMTO               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT ISGMTO_GSTAO_TARIF
               INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM  DB2PRD.TTPO_SGMTO_GSTAO
               WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
               MOVE WRK-AUX-9-3        TO WAGT-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0012               TO WAGT-COD-MSG-ERRO
               MOVE '0012'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOME-TAB
               PERFORM 2111-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE ISGMTO-GSTAO-TARIF     TO WAGT-NM-SEGMTO.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR CURSOR CSR03 OBTER POSTO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-TRATAR-POSTO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-OBTER-DESC-CBANC.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  OBTEM DESCRICAO DO CORRESP. BANCARIO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-OBTER-DESC-CBANC           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA

           MOVE 100                    TO GFCTGB-LL
           MOVE ZEROS                  TO GFCTGB-ZZ

           MOVE WAGS-CAGEN             TO WAGT-CAGEN
                                          GFCTGB-COD-DEPDC
           MOVE WAGS-CPAB              TO WAGT-CPAB
                                          GFCTGB-COD-POSTO

           MOVE WAGS-TRANSACAO         TO GFCTGB-TRANSACAO.
           MOVE WAGS-FUNCAO            TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTGB-QTDE-TOT-REG.
           MOVE WAGS-FUNC-BDSCO        TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS

           INITIALIZE                  GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL  WRK-MODULO         USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE 9                  TO WAGT-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0552'         TO GFCT0M-TRANSACAO

               PERFORM 3000-FINALIZAR
           END-IF

           IF  GFCTGC-ERRO            NOT EQUAL ZEROS
               MOVE 'S'                TO WAGT-FIM
               MOVE GFCTGC-ERRO        TO WAGT-ERRO
               MOVE GFCTGC-COD-SQL-ERRO
                                       TO WAGT-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO
                                       TO WAGT-COD-MSG-ERRO

               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO WAGT-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'
                          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAGT-DESC-MSG-ERRO
               END-IF

               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE GFCTGC-DESC-POSTO(1)   TO WAGT-NM-PAB.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.       *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
