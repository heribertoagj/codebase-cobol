      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1716.
       AUTHOR.     MARCOS JOSE CAVEILEAN.
      *================================================================*
      *                   A L T R A N                                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  DCOM1716                                     *
      *    PROGRAMADOR.:  MARCOS JOSE CAVEILEAN    - ALTRAN            *
      *    ANALISTA....:  MARCELINO                - ALTRAN / GRUPO 70 *
      *    DATA........:  15/01/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ESTORNO DE BAIXA DA PARCELA POR RATEIO -     *
      *                   DETALHES.                                    *
      *----------------------------------------------------------------*
      *    TELA........:  DCOMQPTM - ESTORNO DA BAIXA DA PARCELA POR   *
      *                   RATEIO - DETALHES.                           *
      *----------------------------------------------------------------*

      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE COMUNICACAO COM A BRAD7100               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERE CARACTERES DE NULOS NA MENSAGEM DE SAIDA. *
      *    BRAD7100 - TRATAMENTO DE ERROS.                             *
      *    DCOM5313 - OBTEM LISTA DE PARCELAS COM TOTALIZADOR.         *
      *    POOL5005 - ENVIA / RECEBE MENSAGENS DO IMS.                 *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1715                    CHAMA:  ---       *
      *                  DCOM1717                                      *
      *                  DCOM1718                                      *
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
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.

      *----------------------------------------------------------------*

       77  WRK-MSG001                  PIC  X(079)         VALUE
           'FINAL DE AMOSTRAGEM'.
       77  WRK-MSG002                  PIC  X(079)         VALUE
           'AMOSTRAGEM CONTINUA'.
       77  WRK-MSG003                  PIC  X(079)         VALUE
           'NAO EXISTEM MAIS PAGINAS PARA RETORNAR'.
       77  WRK-MSG004                  PIC  X(079)         VALUE
           'NAO EXISTEM MAIS PAGINAS PARA AVANCAR'.
       77  WRK-MSG005                  PIC  X(079)         VALUE
           'NAO EXISTE(M) PARCELA(S) PARA EXIBIR O DETALHE'.
       77  WRK-MSG006                  PIC  X(079)         VALUE
           'PF INVALIDA'.
       77  WRK-MSG007                  PIC  X(079)         VALUE
           'CONSULTA EFETUADA COM SUCESSO. TECLE <PF3> PARA RETORNAR'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*


       77  IND-1                       PIC  9(002)         VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-PAGINA                  PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-DESPREZAR               PIC  9(005) COMP-3  VALUE ZEROS.

       01  WRK-COD-RET                 PIC +9(004)         VALUE ZEROS.
       01  WRK-COD-RET-R               REDEFINES           WRK-COD-RET.
           05  FILLER                  PIC  X(003).
           05  WRK-COD-RET-N           PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA POOL5005 ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.

       01  WRK-COD-USER.
           05  WRK-COD-USER-N          PIC  9(007)         VALUE ZEROS.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-VERSAO                  PIC  X(006)         VALUE
           'VRS002'.

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*


       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
             10 WRK-COMU-TRANSACAO     PIC  X(008)         VALUE SPACES.
             10 FILLER                 PIC  X(006)         VALUE SPACES.
             10 WRK-COMU-PFK           PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-OPCAO.
             10 WRK-COMU-OPCAO-N       PIC  9(001)         VALUE ZEROS.
           05 WRK-COMU-TIMESTAMP       PIC  X(026)         VALUE SPACES.
           05 WRK-COMU-DADOS.
             10 WRK-COMU-OPERACAO      PIC  9(013)         VALUE ZEROS.
             10 FILLER REDEFINES       WRK-COMU-OPERACAO.
               15 WRK-COMU-DANO-OPER   PIC  9(004).
               15 WRK-COMU-NSEQ-OPER   PIC  9(009).
             10 WRK-COMU-PARCELA       PIC  9(005)         VALUE ZEROS.
             10 WRK-COMU-BANCO         PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-AGENCIA       PIC  9(005)         VALUE ZEROS.
             10 WRK-COMU-CONTA         PIC  9(013)         VALUE ZEROS.

4S2511       10 WRK-COMU-CCNPJ-CPF     PIC  X(09)          VALUE SPACES.
4S2511       10 WRK-COMU-CFLIAL-CNPJ   PIC  X(04)          VALUE SPACES.  
             10 WRK-COMU-CCTRL-CNPJCPF PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-OP-MENU       PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
             10 WRK-COMU-MODAL-BAIXA   PIC  X(001)         VALUE SPACES.
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.
             10 WRK-COMU-CHAMADOR      PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - DCOM5313 ***'.
      *----------------------------------------------------------------*

       01  WRK-5313-ENTRADA.
           05 WRK-5313E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5313E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5313E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5313E-FLAG           PIC  X(001)         VALUE SPACES.

           05 WRK-5313E-START-COUNT    PIC  X(001)         VALUE SPACES.
           05 WRK-5313E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-5313E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-5313E-DANO-OPER      PIC  9(004)         VALUE ZEROS.
           05 WRK-5313E-NSEQ-OPER      PIC  9(009)         VALUE ZEROS.
           05 WRK-5313E-FLAG-FUNCAO    PIC  X(001)         VALUE SPACES.

       01  WRK-5313-SAIDA.
           05 WRK-5313S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-5313S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-5313S-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-5313S-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-5313S-QTDE-RETORNADA PIC  9(003)         VALUE ZEROS.
           05 WRK-5313S-COUNT          PIC  9(009)         VALUE ZEROS.
           05 WRK-5313S-TOTAIS.
             10 WRK-5313S-VPRINC-TOTAL PIC  9(015)V9(002)  VALUE ZEROS.
             10 WRK-5313S-VPSENT-TOTAL PIC  9(015)V9(002)  VALUE ZEROS.
             10 WRK-5313S-VEFETV-TOTAL PIC  9(015)V9(002)  VALUE ZEROS.
             10 WRK-5313S-PEFETV-TOTAL PIC  9(003)V9(002)  VALUE ZEROS.
           05 WRK-5313S-OCORRENCIAS    OCCURS 20 TIMES.
             10 WRK-5313S-NPCELA-DESC  PIC  9(005)         VALUE ZEROS.

             10 WRK-5313S-DVCTO-PCELA  PIC  X(010)         VALUE SPACES.
             10 WRK-5313S-VPRINC-PCELA PIC  9(015)V9(002)  VALUE ZEROS.
             10 WRK-5313S-VPSENT-PCELA PIC  9(015)V9(002)  VALUE ZEROS.
             10 WRK-5313S-VEFETV-BAIXA PIC  9(015)V9(002)  VALUE ZEROS.
             10 WRK-5313S-PEFETV-BAIXA PIC  9(003)V9(002)  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660 '.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD = '.
           05  WRK-RETURN-CODE         PIC +9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL = '.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.


      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQPTM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-QPTM.
           05 FILLER                   PIC  X(018)         VALUE SPACES.
           05 WRK-INP-DADOS-QPTM.
             10 WRK-INP-PFK-QPTM       PIC  X(001)         VALUE SPACES.
             10 WRK-INP-SENHAS-QPTM    PIC  X(037)         VALUE SPACES.
             10 WRK-INP-COMANDO-QPTM   PIC  X(068)         VALUE SPACES.
             10 WRK-INP-TITULO-QPTM    PIC  X(008)         VALUE SPACES.
             10 WRK-INP-PAGINA-QPTM    PIC  9(004)         VALUE ZEROS.
             10 WRK-INP-OCULTOS-QPTM.
               15 WRK-INP-OPCAO-QPTM   PIC  X(001)         VALUE SPACES.
               15 WRK-INP-OPMENU-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-INP-TPMANUT-QPTM PIC  9(001)         VALUE ZEROS.
               15 WRK-INP-TIMEST-QPTM  PIC  X(026)         VALUE SPACES.
4S2511*        15 WRK-INP-NCNPJOC-QPTM PIC  9(009)         VALUE ZEROS.
4S2511         15 WRK-INP-NCNPJOC-QPTM PIC  X(09)          VALUE SPACES.
               15 WRK-INP-CHAMAD1-QPTM PIC  X(008)         VALUE SPACES.

               15 WRK-INP-CHAMAD2-QPTM PIC  X(008)         VALUE SPACES.
               15 WRK-INP-PAGOCUL-QPTM PIC  9(004)         VALUE ZEROS.
               15 WRK-INP-PARCOC-QPTM  PIC  9(005)         VALUE ZEROS.
               15 WRK-INP-FIMAMOS-QPTM PIC  X(001)         VALUE SPACES.
               15 WRK-INP-MODALBX-QPTM PIC  X(001)         VALUE SPACES.
               15 FILLER               PIC  X(014)         VALUE SPACES.
             10 WRK-INP-NOMECLI-QPTM   PIC  X(040)         VALUE SPACES.
             10 WRK-INP-CNPJCPF-QPTM.
4S2511*        15 WRK-INP-CPFNRO-QPTM  PIC  999.999.999    VALUE ZEROS.
4S2511         15 WRK-INP-CPFNRO-QPTM  PIC  X(11)          VALUE SPACES.
               15 WRK-INP-BARRA1-QPTM  PIC  X(001)         VALUE SPACES.
4S2511*        15 WRK-INP-CPFFIL-QPTM  PIC  9(004)         VALUE ZEROS.
4S2511         15 WRK-INP-CPFFIL-QPTM  PIC  X(04)          VALUE SPACES.
               15 WRK-INP-TRACO1-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-INP-CPFCTR-QPTM  PIC  9(002)         VALUE ZEROS.
             10 WRK-INP-CONTA-QPTM.
               15 WRK-INP-BANCO-QPTM   PIC  9(003)         VALUE ZEROS.
               15 WRK-INP-BARRA2-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-INP-AGENCIA-QPTM PIC  9(005)         VALUE ZEROS.
               15 WRK-INP-BARRA3-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-INP-CTA-QPTM     PIC  9(013)         VALUE ZEROS.
             10 WRK-INP-CODPROD-QPTM.
               15 WRK-INP-CODPROD-QPTM-N

                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-INP-DESPROD-QPTM   PIC  X(012)         VALUE SPACES.
             10 WRK-INP-SUBPROD-QPTM.
               15 WRK-INP-SUBPROD-QPTM-N
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-INP-DSSBPROD-QPTM  PIC  X(012)      VALUE SPACES.
             10 WRK-INP-OPERACAO-QPTM.
               15 WRK-INP-DANO-QPTM    PIC  9(004)         VALUE ZEROS.
               15 WRK-INP-NSEQ-QPTM    PIC  9(009)         VALUE ZEROS.
             10 WRK-INP-SITU1-QPTM     PIC  X(015)         VALUE SPACES.
             10 WRK-INP-SITU2-QPTM     PIC  X(013)         VALUE SPACES.
             10 WRK-INP-SITU3-QPTM.
               15 WRK-INP-SITU3-QPTM-N PIC  9(009)         VALUE ZEROS.
             10 WRK-INP-SITU4-QPTM     PIC  X(013)         VALUE SPACES.
             10 WRK-INP-SITU5-QPTM     PIC  X(009)         VALUE SPACES.
             10 WRK-INP-TABELA-QPTM.
               15 FILLER               OCCURS 008 TIMES.
                 20 WRK-INP-NPARC-QPTM PIC  X(005)         VALUE SPACES.
                 20 WRK-INP-DVENC-QPTM PIC  X(010)         VALUE SPACES.
                 20 WRK-INP-VPARC-QPTM PIC  X(016)         VALUE SPACES.
                 20 WRK-INP-VATUA-QPTM PIC  X(016)         VALUE SPACES.

                 20 WRK-INP-VRATE-QPTM PIC  X(016)         VALUE SPACES.
                 20 WRK-INP-PERCE-QPTM PIC  X(006)         VALUE SPACES.
             10 WRK-INP-VLPARTOT-QPTM  PIC  X(016)         VALUE SPACES.
             10 WRK-INP-VLATUTOT-QPTM  PIC  X(016)         VALUE SPACES.
             10 WRK-INP-VLRATTOT-QPTM  PIC  X(016)         VALUE SPACES.
             10 WRK-INP-PERCTOT-QPTM   PIC  X(006)         VALUE SPACES.
             10 WRK-INP-MENSA-QPTM     PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQPTM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-QPTM.
           05 WRK-OUT-LL-QPTM          PIC S9(004)    COMP VALUE ZEROS.
           05 WRK-OUT-ZZ-QPTM          PIC  9(004)    COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-QPTM.
             10 WRK-OUT-SENHAS-QPTM    PIC  X(037)         VALUE SPACES.
             10 WRK-OUT-COMANDO-QPTM   PIC  X(068)         VALUE SPACES.
             10 WRK-OUT-TITULO-QPTM    PIC  X(008)         VALUE SPACES.
             10 WRK-OUT-PAGINA-QPTM    PIC  9(004)         VALUE ZEROS.

             10 WRK-OUT-OCULTOS-QPTM.
               15 WRK-OUT-OPCAO-QPTM   PIC  X(001)         VALUE SPACES.
               15 WRK-OUT-OPMENU-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-OUT-TPMANUT-QPTM PIC  9(001)         VALUE ZEROS.
               15 WRK-OUT-TIMEST-QPTM  PIC  X(026)         VALUE SPACES.
4S2511*        15 WRK-OUT-NCNPJOC-QPTM PIC  9(009)         VALUE ZEROS.
4S2511         15 WRK-OUT-NCNPJOC-QPTM PIC  X(09)          VALUE SPACES.
               15 WRK-OUT-CHAMAD1-QPTM PIC  X(008)         VALUE SPACES.
               15 WRK-OUT-CHAMAD2-QPTM PIC  X(008)         VALUE SPACES.
               15 WRK-OUT-PAGOCUL-QPTM PIC  9(004)         VALUE ZEROS.
               15 WRK-OUT-PARCOC-QPTM  PIC  9(005)         VALUE ZEROS.
               15 WRK-OUT-FIMAMOS-QPTM PIC  X(001)         VALUE SPACES.
               15 WRK-OUT-MODALBX-QPTM PIC  X(001)         VALUE SPACES.
               15 FILLER               PIC  X(014)         VALUE SPACES.
             10 WRK-OUT-NOMECLI-QPTM   PIC  X(040)         VALUE SPACES.
             10 WRK-OUT-CNPJCPF-QPTM.
4S2511*        15 WRK-OUT-CPFNRO-QPTM  PIC  999.999.999    VALUE ZEROS.
4S2511         15 WRK-OUT-CPFNRO-QPTM  PIC  X(11)          VALUE SPACES.
               15 WRK-OUT-BARRA1-QPTM  PIC  X(001)         VALUE SPACES.
4S2511*        15 WRK-OUT-CPFFIL-QPTM  PIC  9(004)         VALUE ZEROS.
4S2511         15 WRK-OUT-CPFFIL-QPTM  PIC  X(04)          VALUE SPACES.
               15 WRK-OUT-TRACO1-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-OUT-CPFCTR-QPTM  PIC  9(002)         VALUE ZEROS.
             10 WRK-OUT-CONTA-QPTM.

               15 WRK-OUT-BANCO-QPTM   PIC  9(003)         VALUE ZEROS.
               15 WRK-OUT-BARRA2-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-OUT-AGENCIA-QPTM PIC  9(005)         VALUE ZEROS.
               15 WRK-OUT-BARRA3-QPTM  PIC  X(001)         VALUE SPACES.
               15 WRK-OUT-CTA-QPTM     PIC  9(013)         VALUE ZEROS.
             10 WRK-OUT-CODPROD-QPTM.
               15 WRK-OUT-CODPROD-QPTM-N
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-OUT-DESPROD-QPTM   PIC  X(012)         VALUE SPACES.
             10 WRK-OUT-SUBPROD-QPTM.
               15 WRK-OUT-SUBPROD-QPTM-N
                                       PIC  9(003)         VALUE ZEROS.
             10 WRK-OUT-DSSBPROD-QPTM  PIC  X(012)         VALUE SPACES.
             10 WRK-OUT-OPERACAO-QPTM.
               15 WRK-OUT-DANO-QPTM    PIC  9(004)         VALUE ZEROS.
               15 WRK-OUT-NSEQ-QPTM    PIC  9(009)         VALUE ZEROS.
             10 WRK-OUT-SITU1-QPTM     PIC  X(015)         VALUE SPACES.
             10 WRK-OUT-SITU2-QPTM     PIC  X(013)         VALUE SPACES.
             10 WRK-OUT-SITU3-QPTM.
               15 WRK-OUT-SITU3-QPTM-N PIC  9(009)         VALUE ZEROS.
             10 WRK-OUT-SITU4-QPTM     PIC  X(013)         VALUE SPACES.

             10 WRK-OUT-SITU5-QPTM     PIC  X(009)         VALUE SPACES.
             10 WRK-OUT-TABELA-QPTM.
               15 FILLER               OCCURS 008 TIMES.
                 20 WRK-OUT-NPARC-QPTM.
                   25 WRK-OUT-NPARC-QPTM-N
                                       PIC  9(005)         VALUE ZEROS.
                 20 WRK-OUT-DVENC-QPTM PIC  X(010)         VALUE SPACES.
                 20 WRK-OUT-VPARC-QPTM.
                   25 WRK-OUT-VPARC-QPTM-N
                                       PIC  ZZZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 20 WRK-OUT-VATUA-QPTM.
                   25 WRK-OUT-VATUA-QPTM-N
                                       PIC  ZZZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 20 WRK-OUT-VRATE-QPTM.
                   25 WRK-OUT-VRATE-QPTM-N
                                       PIC  ZZZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 20 WRK-OUT-PERCE-QPTM.
                   25 WRK-OUT-PERCE-QPTM-N

                                       PIC  ZZ9,99         VALUE ZEROS.
             10 WRK-OUT-VLPARTOT-QPTM.
               15 WRK-OUT-VLPARTOT-QPTM-N
                                       PIC  ZZZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLATUTOT-QPTM.
               15 WRK-OUT-VLATUTOT-QPTM-N
                                       PIC  ZZZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-VLRATTOT-QPTM.
               15 WRK-OUT-VLRATTOT-QPTM-N
                                       PIC  ZZZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-OUT-PERCTOT-QPTM.
               15 WRK-OUT-PERCTOT-QPTM-N
                                       PIC  ZZ9,99         VALUE ZEROS.
             10 WRK-OUT-MENSA-QPTM     PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMQPTM - BRAD0660 - ***'.

      *---------------------------------------------------------------*

       01  WRK-660-DCOMQPTM.
           05 WRK-660-LL-AREA-QPTM     PIC  9(004)    COMP VALUE  0000.
           05 WRK-660-LL-MENSAGEM-QPTM PIC  9(004)    COMP VALUE  0000.
           05 WRK-660-SENHAS-QPTM      PIC  9(004)    COMP VALUE  0037.
           05 WRK-660-COMANDO-QPTM     PIC  9(004)    COMP VALUE  1068.
           05 WRK-660-TITULO-QPTM      PIC  9(004)    COMP VALUE  0008.
           05 WRK-660-PAGINA-QPTM      PIC  9(004)    COMP VALUE  0004.
           05 WRK-660-OCULTOS-QPTM     PIC  9(004)    COMP VALUE  0079.
           05 WRK-660-NOMECLI-QPTM     PIC  9(004)    COMP VALUE  0040.
           05 WRK-660-CNPJCPF-QPTM     PIC  9(004)    COMP VALUE  0019.
           05 WRK-660-CONTA-QPTM       PIC  9(004)    COMP VALUE  0023.
           05 WRK-660-CODPROD-QPTM     PIC  9(004)    COMP VALUE  0003.
           05 WRK-660-DESPROD-QPTM     PIC  9(004)    COMP VALUE  0012.
           05 WRK-660-SUBPROD-QPTM     PIC  9(004)    COMP VALUE  0003.
           05 WRK-660-DSSBPROD-QPTM    PIC  9(004)    COMP VALUE  0012.
           05 WRK-660-OPERACAO-QPTM    PIC  9(004)    COMP VALUE  0013.
           05 WRK-660-SITU1-QPTM       PIC  9(004)    COMP VALUE  0015.
           05 WRK-660-SITU2-QPTM       PIC  9(004)    COMP VALUE  0013.
           05 WRK-660-SITU3-QPTM       PIC  9(004)    COMP VALUE  0009.

           05 WRK-660-SITU4-QPTM       PIC  9(004)    COMP VALUE  0013.
           05 WRK-660-SITU5-QPTM       PIC  9(004)    COMP VALUE  0009.
           05 WRK-660-OCORRENCIAS-QPTM OCCURS 008 TIMES.
              10 WRK-660-NPARC-QPTM    PIC  9(004)    COMP VALUE  0005.
              10 WRK-660-DVENC-QPTM    PIC  9(004)    COMP VALUE  0010.
              10 WRK-660-VPARC-QPTM    PIC  9(004)    COMP VALUE  0016.
              10 WRK-660-VATUA-QPTM    PIC  9(004)    COMP VALUE  0016.
              10 WRK-660-VRATE-QPTM    PIC  9(004)    COMP VALUE  0016.
              10 WRK-660-PERCE-QPTM    PIC  9(004)    COMP VALUE  0006.
           05 WRK-660-VLPARTOT-QPTM    PIC  9(004)    COMP VALUE  0016.
           05 WRK-660-VLATUTOT-QPTM    PIC  9(004)    COMP VALUE  0016.
           05 WRK-660-VLRATTOT-QPTM    PIC  9(004)    COMP VALUE  0016.
           05 WRK-660-PERCTOT-QPTM     PIC  9(004)    COMP VALUE  0006.
           05 WRK-660-MENSA-QPTM       PIC  9(004)    COMP VALUE  0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).

           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      ******************************************************************
      * EFETUA O PROCESSAMENTO DA TRANSACAO                            *

      ******************************************************************
      *----------------------------------------------------------------*
       0000-PROCESSAR-TRANSACAO        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-QPTM
                                          WRK-INCONSIS.


           PERFORM 1000-ROTINA-PRINCIPAL.

           GO TO 0000-PROCESSAR-TRANSACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMQPTM'             TO WRK-TELA.


           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1716'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE

               PERFORM 1200-PROCESSAR-DCOMQPTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOMQPTM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RECEBE CONTROLE DO PROGRAMA CHAMADOR - DCOM7900                *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.


           EVALUATE WRK-COMU-OPCAO
               WHEN '1'
                     MOVE ' INCLUIR'   TO WRK-OUT-TITULO-QPTM

               WHEN '2'
                     MOVE ' EXCLUIR'   TO WRK-OUT-TITULO-QPTM

               WHEN '3'
                     MOVE 'CONSULTA'   TO WRK-OUT-TITULO-QPTM
           END-EVALUATE.

           MOVE 1                      TO WRK-INP-PAGINA-QPTM
                                          WRK-OUT-PAGINA-QPTM.
           MOVE WRK-COMU-OP-MENU       TO WRK-INP-OPMENU-QPTM
                                          WRK-OUT-OPMENU-QPTM.
           MOVE WRK-COMU-OPCAO-N       TO WRK-INP-OPCAO-QPTM
                                          WRK-OUT-OPCAO-QPTM.
           MOVE WRK-COMU-TP-SELECAO    TO WRK-INP-TPMANUT-QPTM
                                          WRK-OUT-TPMANUT-QPTM.
           MOVE WRK-COMU-TIMESTAMP     TO WRK-INP-TIMEST-QPTM
                                          WRK-OUT-TIMEST-QPTM.

           MOVE WRK-COMU-CHAMADOR      TO WRK-INP-CHAMAD1-QPTM
                                          WRK-OUT-CHAMAD1-QPTM.
           MOVE WRK-COMU-TRANSACAO     TO WRK-INP-CHAMAD2-QPTM
                                          WRK-OUT-CHAMAD2-QPTM.
           MOVE WRK-COMU-PAGINA-1      TO WRK-INP-PAGOCUL-QPTM
                                          WRK-OUT-PAGOCUL-QPTM.
           MOVE WRK-COMU-CCNPJ-CPF     TO WRK-INP-NCNPJOC-QPTM
                                          WRK-OUT-NCNPJOC-QPTM
                                          WRK-OUT-CPFNRO-QPTM.
           MOVE WRK-COMU-CFLIAL-CNPJ   TO WRK-INP-CPFFIL-QPTM
                                          WRK-OUT-CPFFIL-QPTM.
           MOVE WRK-COMU-CCTRL-CNPJCPF TO WRK-INP-CPFCTR-QPTM
                                          WRK-OUT-CPFCTR-QPTM.
           MOVE WRK-COMU-BANCO         TO WRK-INP-BANCO-QPTM
                                          WRK-OUT-BANCO-QPTM.
           MOVE WRK-COMU-AGENCIA       TO WRK-INP-AGENCIA-QPTM
                                          WRK-OUT-AGENCIA-QPTM
           MOVE WRK-COMU-CONTA         TO WRK-INP-CTA-QPTM
                                          WRK-OUT-CTA-QPTM.
           MOVE WRK-COMU-DANO-OPER     TO WRK-INP-DANO-QPTM
                                          WRK-OUT-DANO-QPTM.

           MOVE WRK-COMU-NSEQ-OPER     TO WRK-INP-NSEQ-QPTM
                                          WRK-OUT-NSEQ-QPTM.
           MOVE WRK-COMU-PARCELA       TO WRK-INP-PARCOC-QPTM
                                          WRK-OUT-PARCOC-QPTM
           MOVE WRK-COMU-MODAL-BAIXA   TO WRK-INP-MODALBX-QPTM
                                          WRK-OUT-MODALBX-QPTM
           MOVE '/'                    TO WRK-OUT-BARRA1-QPTM
                                          WRK-OUT-BARRA2-QPTM
                                          WRK-OUT-BARRA3-QPTM.
           MOVE '-'                    TO WRK-OUT-TRACO1-QPTM.
           MOVE ZEROS                  TO WRK-DESPREZAR.

           PERFORM 1110-OBTER-CABECALHO.

           IF  WRK-INCONSIS            EQUAL SPACES
               PERFORM 1120-ACESSAR-DCOM5313

               IF  WRK-INCONSIS        EQUAL SPACES
                   MOVE WRK-MSG007     TO WRK-OUT-MENSA-QPTM
               ELSE
                    PERFORM 1220-RETORNAR-CHAMADOR

                    MOVE WRK-5313S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                    MOVE WRK-COMU-AREA TO WRK-MENSAGEM

               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO PARA OBTER DADOS DO CABECALHO DA TELA           *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-CABECALHO            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-SQLCA
                                          ERRO-AREA.


           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR
                      ERRO-AREA.

           MOVE WRK-COD-USER-N         TO 4174-ENT-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM.
           MOVE WRK-INP-DANO-QPTM      TO 4174-ENT-DANO-OPER-DESC.
           MOVE WRK-INP-NSEQ-QPTM      TO 4174-ENT-NSEQ-OPER-DESC.
           MOVE 'DCOM4174'             TO WRK-MODULO.


           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'
                     PERFORM 1111-FORMATAR-CABECALHO

               WHEN '0099'

                     MOVE 'DB2'         TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    PERFORM 1220-RETORNAR-CHAMADOR
                    MOVE 4174-SAI-MSG-RETORNO
                                        TO WRK-COMU-MENSAGEM
                    MOVE WRK-COMU-AREA  TO WRK-MENSAGEM
                    MOVE '*'            TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATAR CABECALHO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1111-FORMATAR-CABECALHO         SECTION.
      *----------------------------------------------------------------*



           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECLI-QPTM.
           MOVE 4174-SAI-CPRODT            TO WRK-OUT-CODPROD-QPTM-N.
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DESPROD-QPTM.
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-OUT-SUBPROD-QPTM-N.
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DSSBPROD-QPTM.
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITU1-QPTM.

           STRING 4174-SAI-CCNPJ-CPF(1:3) '.'
                  4174-SAI-CCNPJ-CPF(4:3) '.'
                  4174-SAI-CCNPJ-CPF(7:3) '/'
                  4174-SAI-CFLIAL-CNPJ    '-'
                  4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE               INTO WRK-OUT-CNPJCPF-QPTM.

           STRING 4174-SAI-CBCO      '/'
                  4174-SAI-CAG-BCRIA '/'
                  4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO WRK-OUT-CONTA-QPTM.


           EVALUATE TRUE
             WHEN 4174-SAI-CCONTR-LIM-DESC      GREATER ZEROS
                  MOVE 'CONTR LIMITE:'          TO WRK-OUT-SITU2-QPTM
                  MOVE 4174-SAI-CCONTR-LIM-DESC TO WRK-OUT-SITU3-QPTM-N

             WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                  4174-SAI-CELMTO-DESC-COML     EQUAL 8
                  MOVE 'CONV GERAL..:'          TO WRK-OUT-SITU2-QPTM
                  MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SITU3-QPTM-N

             WHEN 4174-SAI-CCONTR-CONVE-DESC    GREATER ZEROS AND
                  4174-SAI-CELMTO-DESC-COML     EQUAL 9
                  MOVE 'CONV CLIENTE:'          TO WRK-OUT-SITU2-QPTM
                  MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                TO WRK-OUT-SITU3-QPTM-N
           END-EVALUATE.

           IF  4174-SAI-NSMULA-OPER-DESC   GREATER ZEROS
               IF  WRK-OUT-SITU2-QPTM      EQUAL SPACES
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITU2-QPTM

                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITU3-QPTM
               ELSE
                   MOVE 'SIMULACAO...:'    TO WRK-OUT-SITU4-QPTM
                   MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-SITU5-QPTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM5313           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5313-ENTRADA
                      WRK-5313-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.


           MOVE WRK-DESPREZAR           TO WRK-5313E-RESTART.
           MOVE WRK-COD-USER-N          TO WRK-5313E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM            TO WRK-5313E-CTERM.
           MOVE WRK-INP-DANO-QPTM       TO WRK-5313E-DANO-OPER.
           MOVE WRK-INP-NSEQ-QPTM       TO WRK-5313E-NSEQ-OPER.

           EVALUATE WRK-INP-OPCAO-QPTM
               WHEN 1
                    MOVE 'I'            TO WRK-5313E-FLAG-FUNCAO
               WHEN 2
                    MOVE 'E'            TO WRK-5313E-FLAG-FUNCAO
               WHEN 3
                    MOVE 'C'            TO WRK-5313E-FLAG-FUNCAO
           END-EVALUATE.

           MOVE 'DCOM5313'              TO WRK-MODULO.

           CALL WRK-MODULO              USING WRK-5313-ENTRADA
                                              WRK-5313-SAIDA
                                              ERRO-AREA

                                              WRK-SQLCA.


           EVALUATE  WRK-5313S-COD-RETORNO
               WHEN '0000'
                     PERFORM 1211-MONTAR-TELA

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MONTAR TELA                                                    *

      ******************************************************************
      *----------------------------------------------------------------*
       1211-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*


           PERFORM VARYING IND-1       FROM 1 BY 1
                     UNTIL IND-1       GREATER 8 OR
                   WRK-5313S-NPCELA-DESC      (IND-1) EQUAL ZEROS

                   MOVE WRK-5313S-NPCELA-DESC (IND-1)
                     TO WRK-OUT-NPARC-QPTM-N  (IND-1)

                   MOVE WRK-5313S-DVCTO-PCELA (IND-1)
                     TO WRK-OUT-DVENC-QPTM    (IND-1)

                   MOVE WRK-5313S-VPRINC-PCELA(IND-1)
                     TO WRK-OUT-VPARC-QPTM-N  (IND-1)

                   MOVE WRK-5313S-VPSENT-PCELA(IND-1)
                     TO WRK-OUT-VATUA-QPTM-N  (IND-1)


                   MOVE WRK-5313S-VEFETV-BAIXA(IND-1)
                     TO WRK-OUT-VRATE-QPTM-N  (IND-1)

                   MOVE WRK-5313S-PEFETV-BAIXA(IND-1)
                     TO WRK-OUT-PERCE-QPTM-N  (IND-1)
                   INSPECT WRK-OUT-DVENC-QPTM (IND-1)
                                               REPLACING ALL '.' BY '/'
           END-PERFORM.

           MOVE WRK-5313S-VPRINC-TOTAL TO WRK-OUT-VLPARTOT-QPTM-N.

           MOVE WRK-5313S-VPSENT-TOTAL TO WRK-OUT-VLATUTOT-QPTM-N.

           MOVE WRK-5313S-VEFETV-TOTAL TO WRK-OUT-VLRATTOT-QPTM-N.

           MOVE WRK-5313S-PEFETV-TOTAL TO WRK-OUT-PERCTOT-QPTM-N.

           IF WRK-5313S-NPCELA-DESC(9) EQUAL ZEROS
              MOVE '*'                 TO WRK-OUT-FIMAMOS-QPTM
              MOVE WRK-MSG001          TO WRK-OUT-MENSA-QPTM

           ELSE
              MOVE SPACES              TO WRK-OUT-FIMAMOS-QPTM
              MOVE WRK-MSG002          TO WRK-OUT-MENSA-QPTM
           END-IF.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMQPTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM                 TO WRK-INPUT-QPTM.

           EVALUATE TRUE
             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND
                    WRK-INP-COMANDO-QPTM     EQUAL 'PFK01')  OR

                    WRK-INP-PFK-QPTM         EQUAL 'H'
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
                    MOVE WRK-INP-MENSA-QPTM  TO WRK-OUT-MENSA-QPTM

             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND
                    WRK-INP-COMANDO-QPTM     EQUAL 'PFK03')  OR
                    WRK-INP-PFK-QPTM         EQUAL '3'
                    PERFORM 1220-RETORNAR-CHAMADOR

             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND
                    WRK-INP-COMANDO-QPTM     EQUAL 'PFK05')  OR
                    WRK-INP-PFK-QPTM         EQUAL '5'
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND
                    WRK-INP-COMANDO-QPTM     EQUAL 'PFK07')  OR
                    WRK-INP-PFK-QPTM         EQUAL '7'
                    PERFORM 1240-RETORNAR-PAGINA

             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND

                    WRK-INP-COMANDO-QPTM     EQUAL 'PFK08')  OR
                    WRK-INP-PFK-QPTM         EQUAL '8'
                    PERFORM 1250-AVANCAR-PAGINA

             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND
                    WRK-INP-COMANDO-QPTM     EQUAL 'PFK10')  OR
                    WRK-INP-PFK-QPTM         EQUAL 'A'
                    PERFORM 1260-RETORNAR-MENU-DCOM

             WHEN  (WRK-INP-PFK-QPTM         EQUAL '.'       AND
                    WRK-INP-COMANDO-QPTM     EQUAL SPACES)
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
                    MOVE WRK-MSG007          TO WRK-OUT-MENSA-QPTM

             WHEN   OTHER
                    PERFORM 1210-DEVOLVER-TELA
                    MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
                    MOVE WRK-MSG006          TO WRK-OUT-MENSA-QPTM
           END-EVALUATE.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O PROGRAMA CHAMADOR - DCOM7900                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-INP-OPMENU-QPTM    TO WRK-COMU-OP-MENU.
           MOVE WRK-INP-OPCAO-QPTM     TO WRK-COMU-OPCAO-N.
           MOVE WRK-INP-TPMANUT-QPTM   TO WRK-COMU-TP-SELECAO.
           MOVE WRK-INP-TIMEST-QPTM    TO WRK-COMU-TIMESTAMP.
           MOVE WRK-INP-CHAMAD1-QPTM   TO WRK-COMU-CHAMADOR.

           MOVE WRK-INP-PAGOCUL-QPTM   TO WRK-COMU-PAGINA-1.

           MOVE WRK-INP-NCNPJOC-QPTM   TO WRK-COMU-CCNPJ-CPF.
           MOVE WRK-INP-CPFFIL-QPTM    TO WRK-COMU-CFLIAL-CNPJ.
           MOVE WRK-INP-CPFCTR-QPTM    TO WRK-COMU-CCTRL-CNPJCPF.
           MOVE WRK-INP-BANCO-QPTM     TO WRK-COMU-BANCO.
           MOVE WRK-INP-AGENCIA-QPTM   TO WRK-COMU-AGENCIA.
           MOVE WRK-INP-CTA-QPTM       TO WRK-COMU-CONTA.
           MOVE WRK-INP-DANO-QPTM      TO WRK-COMU-DANO-OPER.
           MOVE WRK-INP-NSEQ-QPTM      TO WRK-COMU-NSEQ-OPER.
           MOVE WRK-INP-MODALBX-QPTM   TO WRK-COMU-MODAL-BAIXA


           MOVE 'DCOM1716'             TO WRK-COMU-TRANSACAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.
           MOVE WRK-INP-CHAMAD2-QPTM   TO WRK-TELA.
           MOVE WRK-CHNG               TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * RETORNAR PARA O SENHAS - MENU DE ROTINAS                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1716'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE '5'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA PAGINA ANTERIOR                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1240-RETORNAR-PAGINA            SECTION.
      *----------------------------------------------------------------*


           PERFORM 1210-DEVOLVER-TELA.

           IF  WRK-INP-PAGINA-QPTM      EQUAL 1
               MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
               MOVE WRK-MSG003          TO WRK-OUT-MENSA-QPTM
               GO TO 1240-99-FIM
           END-IF.

           COMPUTE WRK-DESPREZAR        = (WRK-INP-PAGINA-QPTM - 2) * 8.

           PERFORM 1120-ACESSAR-DCOM5313.


           IF  WRK-5313S-COD-RETORNO    NOT EQUAL '0000'
               MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
               MOVE WRK-5313S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-QPTM
           ELSE
               SUBTRACT 1               FROM WRK-INP-PAGINA-QPTM
           END-IF.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * AVANCAR PARA PAGINA POSTERIOR                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1250-AVANCAR-PAGINA             SECTION.
      *----------------------------------------------------------------*



           PERFORM 1210-DEVOLVER-TELA.

           IF  WRK-INP-FIMAMOS-QPTM     EQUAL '*'
               MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
               MOVE WRK-MSG004          TO WRK-OUT-MENSA-QPTM
               GO TO 1250-99-FIM
           END-IF.

           COMPUTE WRK-DESPREZAR        = WRK-INP-PAGINA-QPTM * 8.

           PERFORM 1120-ACESSAR-DCOM5313.

           IF  WRK-5313S-COD-RETORNO    NOT EQUAL '0000'
               MOVE WRK-INP-TABELA-QPTM TO WRK-OUT-TABELA-QPTM
               MOVE WRK-5313S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-QPTM
           ELSE
               ADD 1                    TO WRK-INP-PAGINA-QPTM
           END-IF.

      *----------------------------------------------------------------*

       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O SENHAS - MENU DE DCOM                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1260-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*


           INITIALIZE WRK-COMU-AREA.

           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE 'DCOM1716'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE 'A'                    TO WRK-COMU-PFK.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * DEVOLVER DADOS DA INPUT PARA OUTPUT (TELA)                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-INP-TITULO-QPTM    TO WRK-OUT-TITULO-QPTM.
           MOVE WRK-INP-PAGINA-QPTM    TO WRK-OUT-PAGINA-QPTM.
           MOVE WRK-INP-OCULTOS-QPTM   TO WRK-OUT-OCULTOS-QPTM.
           MOVE WRK-INP-NOMECLI-QPTM   TO WRK-OUT-NOMECLI-QPTM.
           MOVE WRK-INP-CNPJCPF-QPTM   TO WRK-OUT-CNPJCPF-QPTM.
           MOVE WRK-INP-CONTA-QPTM     TO WRK-OUT-CONTA-QPTM.
           MOVE WRK-INP-CODPROD-QPTM   TO WRK-OUT-CODPROD-QPTM.
           MOVE WRK-INP-DESPROD-QPTM   TO WRK-OUT-DESPROD-QPTM.
           MOVE WRK-INP-SUBPROD-QPTM   TO WRK-OUT-SUBPROD-QPTM.

           MOVE WRK-INP-DSSBPROD-QPTM  TO WRK-OUT-DSSBPROD-QPTM.
           MOVE WRK-INP-OPERACAO-QPTM  TO WRK-OUT-OPERACAO-QPTM.
           MOVE WRK-INP-SITU1-QPTM     TO WRK-OUT-SITU1-QPTM.
           MOVE WRK-INP-SITU2-QPTM     TO WRK-OUT-SITU2-QPTM.
           MOVE WRK-INP-SITU3-QPTM     TO WRK-OUT-SITU3-QPTM.
           MOVE WRK-INP-SITU4-QPTM     TO WRK-OUT-SITU4-QPTM.
           MOVE WRK-INP-SITU5-QPTM     TO WRK-OUT-SITU5-QPTM.
           MOVE WRK-INP-VLPARTOT-QPTM  TO WRK-OUT-VLPARTOT-QPTM.
           MOVE WRK-INP-VLATUTOT-QPTM  TO WRK-OUT-VLATUTOT-QPTM.
           MOVE WRK-INP-VLRATTOT-QPTM  TO WRK-OUT-VLRATTOT-QPTM.
           MOVE WRK-INP-PERCTOT-QPTM   TO WRK-OUT-PERCTOT-QPTM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA POOL660 COMPACTA DADOS PARA DCOMQPTM                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.

      *----------------------------------------------------------------*


           MOVE LENGTH                 OF WRK-OUTPUT-QPTM
                                       TO WRK-OUT-LL-QPTM
                                          WRK-660-LL-MENSAGEM-QPTM.
           MOVE LENGTH                 OF WRK-660-DCOMQPTM
                                       TO WRK-660-LL-AREA-QPTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-QPTM
                                             WRK-660-DCOMQPTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-COD-RET
               MOVE WRK-COD-RET-N      TO WRK-RETURN-CODE
               MOVE 010                TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-QPTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FINALIZA PROGRAMA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      * CHAMADA A BRAD7100 - TRATAMENTO DE ERRO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES OR LOW-VALUES
               MOVE 'SENH7020'         TO ERR-PGM
               MOVE 'DCOM1716'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM1716'         TO ERR-PGM
           END-IF.

           MOVE  WRK-COD-USER          TO ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO ERR-COD-DEPTO.

           CALL 'BRAD7100'             USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA.


           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
