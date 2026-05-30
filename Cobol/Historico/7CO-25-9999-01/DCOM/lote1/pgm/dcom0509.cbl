      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM0509.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                        A L T R A N                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0509                                    *
      *    PROGRAMADOR.:   FSW - ALCIONE                               *
      *    ANALISTA ...:   EMERSON ARTUR FRANCO                        *
      *    ANALISTA.BCO:   EDGARD RODRIGUES                            *
      *    DATA........:   20/09/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ESTORNO DO CONTRATO - OPERACOES POR AGENCIA *
      *                                                                *
      *----------------------------------------------------------------*
      *    TELA........:   DCOMEITM - ESTORNO DO CONTRATO POR AGENCIA  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - TRATAMENTO DE ERROS DA APLICACAO                 *
      *    I#DCOMSY - TRATAMENTO MENSAGENS DO SISTEMS DCOM(ON-LINE)    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS DO IMS/DC.     *
      *    BRAD0660 - COMPACTAR MENSAGENS DE NAVEGACAO E TRANSMISSAO.  *
      *    BRAD7100 - TRATAMENTO DE ERROS DA APLICACAO.                *
      *    DCOM4750 - LISTA CONTRATO POR AGENCIA                       *
      *    DCOM6416 - MODULO DE NEGOCIO - BUSCA NOME AGENCIA           *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *            CHAMADO POR                         CHAMA           *
      *               DCOM0508                       DCOM0511          *
      *                  -                           DCOM0512          *
      *                  -                           DCOM0513          *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *================================================================*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*
       01  FILLER                      PIC  X(032)  VALUE
           '*** INICIO DA WORKING STORAGE **'.
      *----------------------------------------------------------------*
       01  WRK-COMU-LENG               PIC S9(004)  COMP  VALUE +56.
       01  WRK-INCONSIS                PIC  X(001)  VALUE SPACES.
       01  WRK-COUNT                   PIC  9(001)  VALUE ZEROS.
       01  WRK-IND                     PIC  9(002)  VALUE ZEROS.
       01  WRK-INDT                    PIC  9(002)  VALUE ZEROS.
       01  WRK-RESTART                 PIC  9(005)  VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(008)  VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)  VALUE SPACES.
       01  WRK-ENCONTROU               PIC  9(001)  VALUE ZEROS.
           88  NOT-FOUND                            VALUE 1.

       01  WRK-OPERACAO                PIC  9(013)  VALUE ZEROS.
       01  FILLER    REDEFINES    WRK-OPERACAO.
           03  WRK-ANO-OPER            PIC  9(004).
           03  WRK-SEQ-OPER            PIC  9(009).


       01  WRK-MSG02                   PIC  X(079)  VALUE
            'NAO HA OCORRENCIAS PARA SEREM MOSTRADAS'.
      *
      *----------------------------------------------------------------*
      *--- ATRIBUTOS PARA VARIAVEIS DE TELA                        ----*
      *--- ALF - NOR  -DESP - DISP                       (+193)    ----*
      *--- ALF - BRI - DESP - DISP                       (+201)    ----*
      *--- ALF - NOR - PROT - DISP                       (+225)    ----*
      *--- NUM - NOR - DESP - DISP - CURSOR              (+49353)  ----*
      *--- NUM - BRI - DESP - DISP - CURSOR              (+49369)  ----*
      *--- ALF - NOR - DESP - DISP - CURSOR              (+49345)  ----*
      *----------------------------------------------------------------*
       01  WRK-193                     PIC S9(008)  COMP VALUE +193.
       01  WRK-201                     PIC S9(008)  COMP VALUE +201.
       01  WRK-225                     PIC S9(008)  COMP VALUE +225.
       01  WRK-49353                   PIC S9(008)  COMP VALUE +49353.
       01  WRK-49369                   PIC S9(008)  COMP VALUE +49369.
       01  WRK-49345                   PIC S9(008)  COMP VALUE +49345.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '** VARIAVEIS POOL5005 **'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(04)   VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)   VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)   VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)   VALUE  SPACES.
           05  WRK-TELA                PIC  X(08)   VALUE  SPACES.
           05  WRK-VERSAO              PIC  X(06)   VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(07)   VALUE  SPACES.
           05  WRK-COD-DEPTO           PIC  9(06)   VALUE  ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)   VALUE  SPACES.
               10  WRK-TRANSACAO       PIC  X(08)   VALUE  SPACES.
               10  FILLER              PIC  X(1988) VALUE  SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA MENSAGENS DCOM **'.
      *----------------------------------------------------------------*
      *
           COPY 'I#DCOMSY'.
      *
      *================================================================*
       01  FILLER                      PIC  X(050)  VALUE
           '* AREA VARIAVEIS AXILIARES *'.
      *----------------------------------------------------------------*

       01  WRK-TIMESTAMP               PIC  X(026)  VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ERROS APLICACAO *'.
      *----------------------------------------------------------------*
      *
           COPY 'I#BRAD7C'.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ROTINA DCOM4750 *'.
      *----------------------------------------------------------------*
      *
       01  4750E-ROTEADOR.
           03  4750-E-HEADER.
               05  4750-E-COD-RETORNO             PIC  X(004).
               05  4750-E-MSG-RETORNO             PIC  X(079).
               05  4750-E-RESTART                 PIC  9(005).
               05  4750-E-FLAG                    PIC  X(001).
               05  4750-E-START-COUNT             PIC  X(001).
               05  4750-E-CFUNC-BDSCO             PIC  9(009).
               05  4750-E-CTERM                   PIC  X(008).
           03  4750-E-DADOS.
               10  4750-E-FLAG-FILTRO             PIC  X(002).
               10  4750-E-CBCO                    PIC  9(003).
               10  4750-E-CAG-BCRIA               PIC  9(005).
               10  4750-E-CCTA-BCRIA-CLI          PIC  9(013).
               10  4750-E-DINIC-OPER-DESC         PIC  X(010).
4S2511*        10  4750-E-CCNPJ-CPF               PIC  9(009).
4S2511         10  4750-E-CCNPJ-CPF               PIC  X(009).
4S2511*        10  4750-E-CFLIAL-CNPJ             PIC  9(005).
4S2511         10  4750-E-CFLIAL-CNPJ             PIC  X(004).
               10  4750-E-CCTRL-CNPJ-CPF          PIC  9(002).
               10  4750-E-CSIT-DESC-COML          PIC  9(003).

       01  4750S-ROTEADOR.
           03  4750-S-HEADER.
               05  4750-S-COD-RETORNO      PIC  X(004).
               05  4750-S-MSG-RETORNO      PIC  X(079).
               05  4750-S-RESTART          PIC  9(005).
               05  4750-S-FLAG             PIC  X(001).
               05  4750-S-QTDE-RETORNADA   PIC  9(003).
               05  4750-S-COUNT            PIC  9(009).

           03  4750-S-DADOS.
               05  4750-S-TABELA-SAIDA     OCCURS 10  TIMES.
                   10  4750-S-CCONTR                PIC  9(009).
                   10  4750-S-CVRSAO-CONTR-LIM      PIC  9(003).
                   10  4750-S-IPSSOA-DESC-COML      PIC  X(060).
                   10  4750-S-DINIC-VGCIA-CONTR     PIC  X(010).
                   10  4750-S-DVCTO-CONTR-LIM       PIC  X(010).
                   10  4750-S-VCONTR-LIM-DESC       PIC  9(015)V99.
                   10  4750-S-CSIT-DESC-COML        PIC  9(003).
                   10  4750-S-ISIT-DESC-COML        PIC  X(030).
                   10  4750-S-IRSUMO-SIT-DESC       PIC  X(015).
                   10  4750-S-CSGL-SIT-DESC         PIC  X(006).

      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ROTINA DCOM6416 *'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR-6416.
           03  ROTENT-COD-RETORNO-6416                 PIC X(04).
           03  ROTENT-MSG-RETORNO-6416                 PIC X(79).
           03  ROTENT-RESTART-6416                     PIC 9(05).
           03  ROTENT-FLAG-6416                        PIC X(01).
           03  ROTENT-CFUNC-BDSCO-6416                 PIC 9(09).
           03  ROTENT-CTERM-6416                       PIC X(08).
           03  ROTENT-BANCO-6416                       PIC 9(03).
           03  ROTENT-DEPDC-6416                       PIC 9(05).

       01  SAIDA-ROTEADOR-6416.
           03  ROTSAI-HEADER-6416.
               05  ROTSAI-COD-RETORNO-6416             PIC X(04).
               05  ROTSAI-MSG-RETORNO-6416             PIC X(79).
               05  ROTSAI-RESTART-6416                 PIC 9(05).
               05  ROTSAI-FLAG-6416                    PIC X(01).
           03  ROTSAI-DADOS-6416.
               05  ROTSAI-TABELA-SAIDA-6416.
                   07  ROTSAI-AG-DEPDC-6416            PIC 9(05).
                   07  ROTSAI-DG-DEPDC-6416            PIC X(01).
                   07  ROTSAI-NOME-DEPDC-6416          PIC X(40).
                   07  ROTSAI-END-DEPDC-6416           PIC X(30).
                   07  ROTSAI-DIRETORIA-REG-6416       PIC 9(05).
                   07  ROTSAI-DIRETORIA-NOME-6416      PIC X(40).
                   07  ROTSAI-NOME-BANCO-6416          PIC X(40).
                   07  ROTSAI-POLO-SERVICO-6416        PIC 9(05).
                   07  ROTSAI-MUNIC-DEPDC-6416         PIC X(25).
                   07  ROTSAI-UF-6416                  PIC X(02).
                   07  ROTSAI-CEP-6416                 PIC 9(05).
                   07  ROTSAI-CEP-COMPL-6416           PIC 9(03).
                   07  ROTSAI-INDIC-DEPDC-AT-6416      PIC X(01).
                   07  ROTSAI-NATUREZA-DEPDC-6416      PIC X(01).
                   07  ROTSAI-COD-DISTR-CORR-6416      PIC 9(05).
                   07  ROTSAI-DENCRR-DEPDC-BDSCO-6416  PIC X(10).
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ERROS BRAD0660 **'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)  VALUE
              'RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)  VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)  VALUE ZEROS.
           05 FILLER                   PIC  X(011)  VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)  VALUE ZEROS.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '*** AREA COMUNICACAO ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-TPOPCAO        PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.
4S2511*        10  COMU-CNPJCPF        PIC  9(009)         VALUE ZEROS.
4S2511         10  COMU-CNPJCPF        PIC  X(09)          VALUE SPACES.
4S2511*        10  COMU-FILIAL         PIC  9(004)         VALUE ZEROS.
4S2511         10  COMU-FILIAL         PIC  X(04)          VALUE SPACES.
               10  COMU-CONTROLE       PIC  9(002)         VALUE ZEROS.
               10  COMU-BANCO          PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)         VALUE ZEROS.
               10  COMU-OPERACAO       PIC  9(013)         VALUE ZEROS.
               10  FILLER REDEFINES    COMU-OPERACAO.
                   15 COMU-DANO        PIC  9(004).
                   15 COMU-NSEQ        PIC  9(009).
               10  COMU-CONTRATO       PIC  9(009)         VALUE ZEROS.
               10  COMU-VERSAO         PIC  9(003)         VALUE ZEROS.
               10  COMU-PAGINA1        PIC  9(004)         VALUE ZEROS.
               10  COMU-OCCUR          PIC  9(008)         VALUE ZEROS.
           05  COMU-MENSAGEM           PIC  X(079)         VALUE SPACES.
           05  COMU-ACAO               PIC  X(001)         VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA INPUT DCOMEITM **'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-EITM.
           03  FILLER                  PIC X(018)  VALUE SPACES.
           03  WRK-INP-PFK-EITM        PIC X(001)  VALUE SPACES.
           03  WRK-INP-DADOS-EITM.
               05  WRK-INP-SENHAS-EITM PIC  X(037) VALUE SPACES.
               05  WRK-INP-COMANDO-EITM   PIC  X(068) VALUE SPACES.
               05  WRK-INP-ESCOND-EITM.
                   07  WRK-INP-OPCAO-EITM PIC  9(001) VALUE ZEROS.
                   07  WRK-INP-CPAG-EITM  PIC  9(003) VALUE ZEROS.
                   07  WRK-INP-QTIP-EITM  PIC  9(001) VALUE ZEROS.
                   07  WRK-INP-FIMP-EITM  PIC  X(001) VALUE SPACES.
                   07  WRK-INP-VAGO-EITM  PIC  X(024) VALUE SPACES.
               05  WRK-INP-TIMEST-EITM    PIC  X(026) VALUE SPACES.
               05  WRK-INP-PAG-EITM.
                   07  WRK-INP-PAG-EITM-N PIC  9(003) VALUE ZEROS.
               05  WRK-INP-AGENCIA-EITM.
                   07  WRK-INP-AGENCIA-NUM    PIC  9(005) VALUE ZEROS.
               05  WRK-INP-NMAGEN-EITM    PIC  X(040) VALUE SPACES.
               05  WRK-INP-TAB-EITM   OCCURS   10  TIMES.
                   07  WRK-INP-SEL-EITM   PIC  X(001).
                   07  WRK-INP-NUME-EITM  PIC  X(009).
                   07  WRK-INP-VERS-EITM  PIC  X(003).
                   07  WRK-INP-NOME-EITM  PIC  X(018).
                   07  WRK-INP-DTIN-EITM  PIC  X(010).
                   07  WRK-INP-DTVC-EITM  PIC  X(010).
                   07  WRK-INP-VLIM-EITM  PIC  X(014).
                   07  WRK-INP-SITU-EITM  PIC  X(006).
               05  WRK-INP-MSGEM-EITM     PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA OUTPUT DCOMEITM *'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-EITM.
           03  WRK-OUT-LL-EITM         PIC S9(004)  COMP VALUE +0000.
           03  WRK-OUT-ZZ-EITM         PIC S9(004)  COMP VALUE ZEROS.
           03  WRK-OUT-DADOS-EITM.
               05  WRK-OUT-SENHAS-EITM  PIC  X(037) VALUE SPACES.
               05  WRK-OUT-COMANDO-EITM PIC  X(068) VALUE SPACES.
               05  WRK-OUT-ESCOND-EITM.
                   07  WRK-OUT-OPCAO-EITM PIC  9(001) VALUE ZEROS.
                   07  WRK-OUT-CPAG-EITM  PIC  9(003) VALUE ZEROS.
                   07  WRK-OUT-QTIP-EITM  PIC  9(001) VALUE ZEROS.
                   07  WRK-OUT-FIMP-EITM  PIC  X(001) VALUE SPACES.
                   07  WRK-OUT-VAGO-EITM  PIC  X(024) VALUE SPACES.
               05  WRK-OUT-TIMEST-EITM     PIC  X(026) VALUE SPACES.
               05  WRK-OUT-PAG-EITM.
                   07  WRK-OUT-PAG-EITM-N PIC 9(03) VALUE ZEROS.
               05  WRK-OUT-AGENCIA-EITM PIC  X(005) VALUE ZEROS.
               05  WRK-OUT-AGENCIA-NUM  REDEFINES  WRK-OUT-AGENCIA-EITM
                                        PIC  9(005).
               05  WRK-OUT-NMAGEN-EITM  PIC  X(040) VALUE SPACES.
               05  WRK-OUT-TAB-EITM   OCCURS   10  TIMES.
                   07  WRK-OUT-SEL-ATTR   PIC S9(004) COMP.
                   07  WRK-OUT-SEL-EITM   PIC  X(001).
                   07  WRK-OUT-NUME-EITM  PIC  X(009).
                   07  WRK-OUT-VERS-EITM  PIC  X(003).
                   07  WRK-OUT-NOME-EITM  PIC  X(018).
                   07  WRK-OUT-DTIN-EITM  PIC  X(010).
                   07  WRK-OUT-DTVC-EITM  PIC  X(010).
                   07  WRK-OUT-VLIM-EITM  PIC  X(014).
                   07  WRK-OUT-VLIM-EDIT  REDEFINES  WRK-OUT-VLIM-EITM
                                          PIC  ZZZ.ZZZ.ZZ9,99.
                   07  WRK-OUT-SITU-EITM  PIC  X(006).
               05  WRK-OUT-MSGEM-EITM     PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA BRAD0660 (EITM) *'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMEITM.
           05  WRK-660-LL-AREA-EITM        PIC  9(004) COMP VALUE  0000.
           05  WRK-660-LL-MENSAGEM-EITM    PIC  9(004) COMP VALUE  0000.
           05  WRK-660-SENHAS-EITM         PIC  9(004) COMP VALUE  0037.
           05  WRK-660-ESCOND-EITM         PIC  9(004) COMP VALUE  0030.
           05  WRK-660-TIM-EITM            PIC  9(004) COMP VALUE  0026.
           05  WRK-660-PAG-EITM            PIC  9(004) COMP VALUE  0003.
           05  WRK-660-COMANDO-EITM        PIC  9(004) COMP VALUE  1068.
           05  WRK-660-AGENCIA-EITM        PIC  9(004) COMP VALUE  0005.
           05  WRK-660-NMAGEN-EITM         PIC  9(004) COMP VALUE  0040.
           05  WRK-660-LINHA               OCCURS 10.
               10  WRK-660-SEL-EITM        PIC  9(004) COMP VALUE  2003.
               10  WRK-660-NUME-EITM       PIC  9(004) COMP VALUE  0009.
               10  WRK-660-VERS-EITM       PIC  9(004) COMP VALUE  0003.
               10  WRK-660-NOME-EITM       PIC  9(004) COMP VALUE  0018.
               10  WRK-660-DTIN-EITM       PIC  9(004) COMP VALUE  0010.
               10  WRK-660-DTVC-EITM       PIC  9(004) COMP VALUE  0010.
               10  WRK-660-VLIM-EITM       PIC  9(004) COMP VALUE  0014.
               10  WRK-660-SITU-EITM       PIC  9(004) COMP VALUE  0006.
           05  WRK-660-MSGEM-EITM          PIC  9(004) COMP VALUE  0079.
      *
      *================================================================*
       01  FILLER                      PIC  X(032)  VALUE
           '*** FINAL DA WORKING STORAGE ***'.
      *----------------------------------------------------------------*
      *================================================================*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING       LNK-IO-PCB
                                                   LNK-ALT-PCB.

           MOVE    WRK-GU              TO          WRK-FUNCAO.

      ******************************************************************
      *    SECTION TRATAMENTO INICIAL DO PROGRAMA                      *
      ******************************************************************
       0000-00-INICIAR                 SECTION.
      *----------------------------------------------------------------*

           CALL   'POOL5005'           USING       WRK-VERSAO
                                                   WRK-FUNCAO
                                                   LNK-IO-PCB
                                                   LNK-ALT-PCB
                                                   WRK-MENSAGEM
                                                   WRK-TELA
                                                   WRK-COD-USER
                                                   WRK-COD-DEPTO.

           IF      RETURN-CODE         EQUAL       04  OR  12
                   PERFORM             9999-00-FINALIZAR
           END-IF.

           PERFORM 0100-00-PROCESSAR-MENSAGEM.

           GO      0000-00-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO IDENTIFICACAO DA MENSAGEM RECEBIDA       *
      ******************************************************************
       0100-00-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE    SPACES              TO          WRK-OUT-DADOS-EITM
                                                   WRK-INCONSIS.

           INITIALIZE                              4750E-ROTEADOR
                                                   4750S-ROTEADOR.

           MOVE    'DCOMEITM'          TO          WRK-TELA.

           INITIALIZE          COMU-AREA.

           IF      WRK-TRANSACAO       NOT EQUAL  'DCOM0509'
                   PERFORM             0200-00-RECEBER-CONTROLE
           ELSE
                   PERFORM             0500-00-PROCESSAR-DCOM0509
           END-IF.

           IF      WRK-TELA            EQUAL      'DCOMEITM'
                   PERFORM             9500-00-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RECEBIMENTO DE MENSAGEM OUTROS           *
      ******************************************************************
       0200-00-RECEBER-CONTROLE        SECTION.
      *----------------------------------------------------------------*

           MOVE    SPACES              TO        WRK-INCONSIS.
           MOVE    WRK-MENSAGEM        TO        COMU-AREA.
           MOVE    COMU-TPOPCAO        TO        WRK-INP-OPCAO-EITM
                                                 WRK-OUT-OPCAO-EITM.
           MOVE    COMU-TIMESTAMP      TO        WRK-OUT-TIMEST-EITM
                                                 WRK-INP-TIMEST-EITM.

           IF  WRK-TRANSACAO           EQUAL 'DCOM0508'
               MOVE  1                 TO  WRK-INP-PAG-EITM-N
           ELSE
               IF  COMU-PAGINA1            GREATER 1
                   COMPUTE 4750-E-RESTART  = (COMU-PAGINA1 - 1) * 10
                   MOVE  COMU-PAGINA1      TO  WRK-INP-PAG-EITM-N
                                               WRK-OUT-PAG-EITM-N
               ELSE
                   MOVE  1                 TO  WRK-INP-PAG-EITM-N
                                               WRK-OUT-PAG-EITM-N
               END-IF
           END-IF.

           MOVE    COMU-AGENCIA        TO          WRK-INP-AGENCIA-NUM
                                                   WRK-OUT-AGENCIA-NUM.

           PERFORM 0230-00-CH-DCOM6416.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0200-99-FIM
           END-IF.

           PERFORM 0210-00-CH-DCOM4750.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0200-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           MOVE  WRK-INP-ESCOND-EITM      TO  WRK-OUT-ESCOND-EITM.
           MOVE  WRK-INP-PAG-EITM-N       TO  WRK-OUT-PAG-EITM-N.

           IF  WRK-TRANSACAO               EQUAL     'DCOM0511' OR
                                                     'DCOM0512' OR
                                                     'DCOM0513'
               IF  COMU-MENSAGEM           NOT EQUAL  SPACES
                   MOVE    COMU-MENSAGEM          TO  WRK-OUT-MSGEM-EITM
               ELSE
                   MOVE    SG-MENSAGEM(217)       TO  WRK-OUT-MSGEM-EITM
               END-IF
           ELSE
               MOVE    SG-MENSAGEM(217)    TO  WRK-OUT-MSGEM-EITM
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA DCOM4750                *
      ******************************************************************
       0210-00-CH-DCOM4750             SECTION.
      *----------------------------------------------------------------*

           EVALUATE  WRK-INP-OPCAO-EITM
              WHEN 1
                 MOVE 'IE'                   TO 4750-E-FLAG-FILTRO
              WHEN 2
                 MOVE 'EE'                   TO 4750-E-FLAG-FILTRO
              WHEN 3
                 MOVE 'CE'                   TO 4750-E-FLAG-FILTRO
           END-EVALUATE

           MOVE 237                    TO 4750-E-CBCO.
           MOVE WRK-INP-AGENCIA-NUM    TO 4750-E-CAG-BCRIA
           MOVE WRK-COD-USER           TO 4750-E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4750-E-CTERM
           MOVE   'DCOM4750'           TO          WRK-MODULO.

           CALL    WRK-MODULO          USING       4750E-ROTEADOR
                                                   4750S-ROTEADOR
                                                   ERRO-AREA
                                                   WRK-SQLCA.

           EVALUATE  4750-S-COD-RETORNO

             WHEN '0000'
             WHEN '0002'
                   IF   4750-S-CCONTR(10)          EQUAL ZEROS AND
                        4750-S-FLAG                EQUAL 'S'
                        MOVE 4750-S-FLAG      TO   WRK-INP-FIMP-EITM
                                                   WRK-OUT-FIMP-EITM
                   ELSE
                        MOVE SPACES           TO   WRK-INP-FIMP-EITM
                                                   WRK-OUT-FIMP-EITM
                   END-IF
             WHEN  '0003'
                   MOVE '*'                    TO WRK-INCONSIS
                   IF  COMU-PAGINA1            EQUAL 1 AND
                       COMU-OCCUR              EQUAL 1 AND
                       COMU-ACAO               EQUAL 'S'

                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-EITM
                   ELSE
                       MOVE WRK-MSG02          TO  COMU-MENSAGEM
                   END-IF
                   PERFORM             0520-00-RETORNAR-CHAMADOR

             WHEN '0099'
                   MOVE  'DB2'         TO          ERR-TIPO-ACESSO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO

             WHEN  OTHER
                   MOVE '*'                    TO WRK-INCONSIS
                   IF  COMU-PAGINA1            EQUAL 1 AND
                       COMU-OCCUR              EQUAL 1 AND
                       COMU-ACAO               EQUAL 'S'

                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-EITM
                   ELSE
                       MOVE 4750-S-MSG-RETORNO TO  COMU-MENSAGEM
                   END-IF
                   PERFORM             0520-00-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       0210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA DCOM6416 (NOME AGENCIA) *
      ******************************************************************
       0230-00-CH-DCOM6416             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  ENTRADA-ROTEADOR-6416.

           MOVE WRK-COD-USER           TO ROTENT-CFUNC-BDSCO-6416
           MOVE LNK-IO-LTERM           TO ROTENT-CTERM-6416
           MOVE 237                    TO ROTENT-BANCO-6416
           MOVE WRK-INP-AGENCIA-NUM    TO ROTENT-DEPDC-6416
           MOVE 'N'                    TO ROTENT-FLAG-6416

           MOVE 'DCOM6416'             TO WRK-MODULO
           CALL WRK-MODULO             USING ENTRADA-ROTEADOR-6416
                                             SAIDA-ROTEADOR-6416
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-COD-RETORNO-6416
              WHEN '0000'
                   MOVE ROTSAI-NOME-DEPDC-6416
                                            TO WRK-INP-NMAGEN-EITM
                                               WRK-OUT-NMAGEN-EITM

               WHEN '0099'
                    MOVE 'DB2'               TO ERR-TIPO-ACESSO
                    PERFORM 9995-00-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE ROTSAI-MSG-RETORNO-6416
                                       TO COMU-MENSAGEM
                   PERFORM             0520-00-RETORNAR-CHAMADOR

            END-EVALUATE.

      *----------------------------------------------------------------*
       0230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION PARA MONTAGEM DE OCORRENCIAS NA TELA                *
      ******************************************************************
       3000-00-MONTA-TELA              SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 10

             IF   4750-S-CCONTR(WRK-IND)  GREATER ZEROS
                  MOVE  WRK-193       TO      WRK-OUT-SEL-ATTR(WRK-IND)
                  MOVE 4750-S-CCONTR(WRK-IND)
                       TO            WRK-OUT-NUME-EITM(WRK-IND)
                  MOVE 4750-S-CVRSAO-CONTR-LIM(WRK-IND)
                       TO            WRK-OUT-VERS-EITM(WRK-IND)
                  MOVE 4750-S-IPSSOA-DESC-COML(WRK-IND)
                       TO            WRK-OUT-NOME-EITM(WRK-IND)
                  MOVE 4750-S-DINIC-VGCIA-CONTR(WRK-IND)
                       TO            WRK-OUT-DTIN-EITM(WRK-IND)
                  MOVE 4750-S-DVCTO-CONTR-LIM(WRK-IND)
                       TO            WRK-OUT-DTVC-EITM(WRK-IND)
                  MOVE 4750-S-VCONTR-LIM-DESC(WRK-IND)
                       TO            WRK-OUT-VLIM-EDIT(WRK-IND)
                  MOVE 4750-S-CSGL-SIT-DESC(WRK-IND)
                       TO            WRK-OUT-SITU-EITM(WRK-IND)

                  INSPECT WRK-OUT-DTIN-EITM(WRK-IND)
                          REPLACING ALL '.' BY '/'
                  INSPECT WRK-OUT-DTVC-EITM(WRK-IND)
                          REPLACING ALL '.' BY '/'
             ELSE
                  MOVE  WRK-225       TO  WRK-OUT-SEL-ATTR(WRK-IND)
                  MOVE SPACES         TO  WRK-OUT-NUME-EITM(WRK-IND)
                                          WRK-OUT-VERS-EITM(WRK-IND)
                                          WRK-OUT-NOME-EITM(WRK-IND)
                                          WRK-OUT-DTIN-EITM(WRK-IND)
                                          WRK-OUT-DTVC-EITM(WRK-IND)
                                          WRK-OUT-VLIM-EITM(WRK-IND)
                                          WRK-OUT-SITU-EITM(WRK-IND)
             END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RECEBIMENTO DE MENSAGEM DO PROGRAMA      *
      ******************************************************************
       0500-00-PROCESSAR-DCOM0509      SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM        TO          WRK-INPUT-EITM.
           PERFORM 0510-00-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EITM     EQUAL 'H' OR
                   (WRK-INP-PFK-EITM     EQUAL '.' AND
                    WRK-INP-COMANDO-EITM EQUAL 'PFK01')
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-INP-MSGEM-EITM     TO WRK-OUT-MSGEM-EITM
                    IF  WRK-OUT-NUME-EITM(1)    NOT EQUAL ZEROS
                        MOVE WRK-49345 TO WRK-OUT-SEL-ATTR(1)
                    END-IF

               WHEN WRK-INP-PFK-EITM     EQUAL '3' OR
                   (WRK-INP-PFK-EITM     EQUAL '.' AND
                    WRK-INP-COMANDO-EITM EQUAL 'PFK03')
                    PERFORM 0520-00-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EITM     EQUAL 'X' OR
                   (WRK-INP-PFK-EITM     EQUAL '.' AND
                    WRK-INP-COMANDO-EITM EQUAL 'PFK05')
                    PERFORM 0530-00-MENU-ROTINAS

               WHEN WRK-INP-PFK-EITM     EQUAL 'V' OR
                   (WRK-INP-PFK-EITM     EQUAL '.' AND
                    WRK-INP-COMANDO-EITM EQUAL 'PFK10')
                    PERFORM 0540-00-MENU-DCOM

               WHEN WRK-INP-PFK-EITM     EQUAL '7' OR
                   (WRK-INP-PFK-EITM     EQUAL '.' AND
                    WRK-INP-COMANDO-EITM EQUAL 'PFK07')
                    PERFORM 0550-00-TELA-ANTERIOR

               WHEN WRK-INP-PFK-EITM     EQUAL '8' OR
                   (WRK-INP-PFK-EITM     EQUAL '.' AND
                    WRK-INP-COMANDO-EITM EQUAL 'PFK08')
                    PERFORM 0560-00-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-EITM          EQUAL '.' AND
                     WRK-INP-COMANDO-EITM      EQUAL SPACES)
                    PERFORM 0511-00-DEVOLVER-OCORR
                    PERFORM 0570-00-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-49345         TO WRK-OUT-SEL-ATTR(1)
                    MOVE SG-MENSAGEM(38)   TO WRK-OUT-MSGEM-EITM

           END-EVALUATE.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0510-00-DEVOLVER-TELA           SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-INP-ESCOND-EITM   TO        WRK-OUT-ESCOND-EITM.
           MOVE    WRK-INP-PAG-EITM      TO        WRK-OUT-PAG-EITM.
           MOVE    WRK-INP-AGENCIA-EITM  TO        WRK-OUT-AGENCIA-EITM.
           MOVE    WRK-INP-NMAGEN-EITM   TO        WRK-OUT-NMAGEN-EITM.
           MOVE    WRK-INP-TIMEST-EITM   TO        WRK-OUT-TIMEST-EITM.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0511-00-DEVOLVER-OCORR          SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 10

               IF  WRK-INP-NUME-EITM(WRK-IND)    NOT EQUAL SPACES
                   MOVE  WRK-INP-SEL-EITM(WRK-IND)
                                          TO  WRK-OUT-SEL-EITM(WRK-IND)
                   MOVE  WRK-INP-NUME-EITM(WRK-IND)
                                          TO  WRK-OUT-NUME-EITM(WRK-IND)
                   MOVE  WRK-INP-VERS-EITM(WRK-IND)
                                          TO  WRK-OUT-VERS-EITM(WRK-IND)
                   MOVE  WRK-INP-NOME-EITM(WRK-IND)
                                          TO  WRK-OUT-NOME-EITM(WRK-IND)
                   MOVE  WRK-INP-DTIN-EITM(WRK-IND)
                                          TO  WRK-OUT-DTIN-EITM(WRK-IND)
                   MOVE  WRK-INP-DTVC-EITM(WRK-IND)
                                          TO  WRK-OUT-DTVC-EITM(WRK-IND)
                   MOVE  WRK-INP-VLIM-EITM(WRK-IND)
                                          TO  WRK-OUT-VLIM-EITM(WRK-IND)
                   MOVE  WRK-INP-SITU-EITM(WRK-IND)
                                          TO  WRK-OUT-SITU-EITM(WRK-IND)
               ELSE
                   MOVE  WRK-225       TO  WRK-OUT-SEL-ATTR(WRK-IND)
               END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       0511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO AO PROGRAMA CHAMADOR             *
      ******************************************************************
       0520-00-RETORNAR-CHAMADOR       SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-INP-TIMEST-EITM   TO   COMU-TIMESTAMP.
           MOVE    237                   TO   COMU-BANCO
           MOVE    WRK-INP-AGENCIA-NUM   TO   COMU-AGENCIA
           MOVE   'DCOM0509'             TO   COMU-TRANSACAO
           MOVE    LENGTH                OF   COMU-AREA
                                         TO   COMU-LL.
           MOVE   'DCOM0508'             TO   WRK-TELA.
           MOVE    WRK-CHNG              TO   WRK-FUNCAO.
           MOVE    COMU-AREA             TO   WRK-MENSAGEM.

      *----------------------------------------------------------------*
       0520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO AO MENU ROTINAS DO SENHAS        *
      ******************************************************************
       0530-00-MENU-ROTINAS            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE  LENGTH OF COMU-AREA   TO          COMU-LL.
           MOVE   'DCOM0509'           TO          COMU-TRANSACAO
                                                   WRK-TELA.
           MOVE   '5'                  TO          COMU-PFK.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.
           MOVE    COMU-AREA           TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       0530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO AO MENU SISTEMA DCOM             *
      ******************************************************************
       0540-00-MENU-DCOM               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE LENGTH OF COMU-AREA    TO          COMU-LL.
           MOVE   'DCOM0509'           TO          COMU-TRANSACAO
                                                   WRK-TELA.
           MOVE   'A'                  TO          COMU-PFK.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.
           MOVE    COMU-AREA           TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       0540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO DE TELA NA PAGINACAO             *
      ******************************************************************
       0550-00-TELA-ANTERIOR           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAG-EITM-N      EQUAL 1
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MSGEM-EITM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0550-99-FIM
           END-IF.

           COMPUTE 4750-E-RESTART      = (WRK-INP-PAG-EITM-N - 2) * 10

           PERFORM 0210-00-CH-DCOM4750.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0550-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           MOVE SG-MENSAGEM(01)        TO WRK-OUT-MSGEM-EITM
           COMPUTE WRK-OUT-PAG-EITM-N  = WRK-INP-PAG-EITM-N - 1.

      *----------------------------------------------------------------*
       0550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO AVANCO DE TELA NA PAGINACAO              *
      ******************************************************************
       0560-00-TELA-POSTERIOR          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMP-EITM       EQUAL 'S'
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MSGEM-EITM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0560-99-FIM
           END-IF.

           COMPUTE 4750-E-RESTART       = (WRK-INP-PAG-EITM-N * 10)

           PERFORM 0210-00-CH-DCOM4750.
           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0560-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           IF   WRK-INP-FIMP-EITM       EQUAL 'S'
                MOVE SG-MENSAGEM(17)    TO WRK-OUT-MSGEM-EITM
           ELSE
                MOVE SG-MENSAGEM(01)    TO WRK-OUT-MSGEM-EITM
           END-IF

           COMPUTE WRK-OUT-PAG-EITM-N   = WRK-INP-PAG-EITM-N + 1.

      *----------------------------------------------------------------*
       0560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO PROCESSAMENTO PROGRAMA ( ENTER )         *
      ******************************************************************
       0570-00-TRATAR-ENTER            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0571-00-CONSISTENCIAS.

           IF      WRK-INCONSIS        EQUAL SPACES
                   PERFORM             0572-00-TRANSFERIR-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    ROTINA PARA CONSISTENCIA DOS ARGUMENTOS DE PESQUISA         *
      ******************************************************************
       0571-00-CONSISTENCIAS           SECTION.
      *----------------------------------------------------------------*

           MOVE    ZEROS               TO          WRK-COUNT.
           MOVE    SPACES              TO          WRK-INCONSIS.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 10

             IF    WRK-INP-SEL-EITM(WRK-IND)
                                       NOT EQUAL   SPACES AND LOW-VALUES

                   MOVE  WRK-49353     TO  WRK-OUT-SEL-ATTR(WRK-IND)
               IF  WRK-INP-SEL-EITM(WRK-IND)       EQUAL  'X'
                   ADD   1             TO          WRK-COUNT
                   MOVE  WRK-IND       TO          WRK-INDT
               ELSE
                   MOVE '*'            TO          WRK-INCONSIS
                   MOVE  WRK-49369     TO  WRK-OUT-SEL-ATTR(WRK-IND)
               END-IF
             END-IF

           END-PERFORM.

           IF      WRK-INCONSIS        EQUAL       '*'
                   MOVE SG-MENSAGEM(086)   TO      WRK-OUT-MSGEM-EITM
           ELSE
            IF     WRK-COUNT           EQUAL       0
                   MOVE  '*'           TO          WRK-INCONSIS
                   MOVE SG-MENSAGEM(0217)  TO      WRK-OUT-MSGEM-EITM
            ELSE
             IF    WRK-COUNT           GREATER     1
                   MOVE  '*'           TO          WRK-INCONSIS
                   MOVE SG-MENSAGEM(0215)  TO      WRK-OUT-MSGEM-EITM
             END-IF
            END-IF
           END-IF.

      *----------------------------------------------------------------*
       1010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO CONTROLE TRANSFERENCIA OUTROS PROGRAMAS  *
      ******************************************************************
       0572-00-TRANSFERIR-CONTROLE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE    WRK-INP-OPCAO-EITM          TO  COMU-TPOPCAO
           MOVE    WRK-INP-TIMEST-EITM         TO  COMU-TIMESTAMP
           MOVE   'DCOM0509'                   TO  COMU-TRANSACAO

           MOVE    LENGTH  OF  COMU-AREA
                                               TO  COMU-LL
           MOVE    237                         TO  COMU-BANCO
           MOVE    WRK-INP-AGENCIA-NUM         TO  COMU-AGENCIA
           MOVE    WRK-INP-PAG-EITM-N          TO  COMU-PAGINA1

           IF      WRK-INP-PAG-EITM-N          EQUAL 1  AND
                   WRK-INP-NUME-EITM(2)        EQUAL SPACES OR
                                                     LOW-VALUES
                   MOVE 1                      TO  COMU-OCCUR
           END-IF

           MOVE    WRK-INP-NUME-EITM(WRK-INDT) TO  COMU-CONTRATO
           MOVE    WRK-INP-VERS-EITM(WRK-INDT) TO  COMU-VERSAO.

           EVALUATE  WRK-INP-OPCAO-EITM
              WHEN  1
                    MOVE 'DCOM0511'       TO  WRK-TELA
              WHEN  2
                    MOVE 'DCOM0512'       TO  WRK-TELA
              WHEN  3
                    MOVE 'DCOM0513'       TO  WRK-TELA
              WHEN  OTHER
                    MOVE SG-MENSAGEM(35)  TO  WRK-OUT-MSGEM-EITM
           END-EVALUATE

           MOVE    COMU-AREA           TO          WRK-MENSAGEM.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.

      *----------------------------------------------------------------*
       0572-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA BRAD0660 (TELA MFS)     *
      ******************************************************************
       9500-00-PROCESSAR-BRAD0660      SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-EITM
                                       TO  WRK-660-LL-MENSAGEM-EITM
                                           WRK-OUT-LL-EITM

           MOVE LENGTH OF WRK-660-DCOMEITM
                                       TO  WRK-660-LL-AREA-EITM

           CALL   'BRAD0660'           USING       WRK-OUTPUT-EITM
                                                   WRK-660-DCOMEITM.

           IF      RETURN-CODE         NOT EQUAL   ZEROS
                   MOVE 'APL'          TO          ERR-TIPO-ACESSO
                   MOVE  RETURN-CODE   TO          WRK-RETURN-CODE
                   MOVE  20            TO          WRK-LOCAL-ERRO
                   MOVE  WRK-ERRO-BRAD0660 TO      ERR-TEXTO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE    WRK-ISRT            TO          WRK-FUNCAO.
           MOVE    WRK-OUTPUT-EITM     TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA BRAD7100 (ABENDS)       *
      ******************************************************************
       9995-00-PROCESSAR-ROTINA-ERRO   SECTION.
      *----------------------------------------------------------------*

           IF      ERR-MODULO          NOT EQUAL   SPACES AND LOW-VALUES
                   MOVE 'DCOM0509'     TO          ERR-PGM
           ELSE
                   MOVE 'SENHAS02'     TO          ERR-PGM
                   MOVE 'DCOM0509'     TO          ERR-MODULO
           END-IF.

           MOVE    WRK-COD-USER        TO          ERR-COD-USER.
           MOVE    WRK-COD-DEPTO       TO          ERR-COD-DEPTO.

           IF      ERR-TIPO-ACESSO     EQUAL      'DB2'
                   CALL 'BRAD7100'     USING       LNK-IO-PCB
                                                   ERRO-AREA
                                                   LNK-ALT-PCB
                                                   WRK-SQLCA
           ELSE
                   CALL 'BRAD7100'     USING       LNK-IO-PCB
                                                   ERRO-AREA
                                                   LNK-ALT-PCB
           END-IF.

           PERFORM 9999-00-FINALIZAR.

      *----------------------------------------------------------------*
       9995-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-00-FINALIZAR               SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
