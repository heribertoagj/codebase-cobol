      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM0503.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                        A L T R A N                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0503                                    *
      *    PROGRAMADOR.:   FSW - ALCIONE                               *
      *    ANALISTA ...:   EMERSON ARTUR FRANCO                        *
      *    ANALISTA.BCO:   EDGARD RODRIGUES                            *
      *    DATA........:   22/04/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ESTORNO DE OPERACAO - OPERACOES POR AGENCIA *
      *                                                                *
      *----------------------------------------------------------------*
      *    TELA........:   DCOMECTM - ESTORNO DA OPERACAO POR AGENCIA  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - TRATAMENTO DE ERROS DA APLICACAO                 *
      *    I#DCOMSY - TRATAMENTO MENSAGENS DO SISTEMS DCOM(ON-LINE)    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS DO IMS/DC.     *
      *    BRAD0660 - COMPACTAR MENSAGENS DE NAVEGACAO E TRANSMISSAO.  *
      *    BRAD7100 - TRATAMENTO DE ERROS DA APLICACAO.                *
      *    DCOM4310 - MODULO DE NEGOCIO - EXCLUSAO - CONSULTA.         *
      *    DCOM4370 - MODULO DE NEGOCIO - INCLUSAO                     *
      *    DCOM6416 - MODULO DE NEGOCIO - BUSCA NOME AGENCIA           *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *            CHAMADO POR                         CHAMA           *
      *               DCOM0502                       DCOM0505          *
      *                  -                           DCOM0506          *
      *                  -                           DCOM0507          *
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
       01  WIND                        PIC  9(002)  VALUE ZEROS.
       01  WINDT                       PIC  9(002)  VALUE ZEROS.
       01  WRK-RESTART                 PIC  9(005)  VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(008)  VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)  VALUE SPACES.
       01  WRK-ENCONTROU               PIC  9(001)  VALUE ZEROS.
           88  NOT-FOUND                            VALUE 1.
       01  WRK-PRODUTO.
           05  WRK-COD-PROD            PIC  9(03)    VALUE ZEROS.
           05  WRK-BARRA-PROD          PIC  X(01)    VALUE SPACES.
           05  WRK-DESC-PROD           PIC  X(15)    VALUE SPACES.

       01  WRK-SUBPROD.
           05  WRK-COD-SUBP            PIC  9(03)    VALUE ZEROS.
           05  WRK-BARRA-SUBP          PIC  X(01)    VALUE SPACES.
           05  WRK-DESC-SUBP           PIC  X(15)    VALUE SPACES.

       01  WRK-OPERACAO                PIC  9(013)  VALUE ZEROS.
       01  FILLER    REDEFINES    WRK-OPERACAO.
           03  WRK-ANO-OPER            PIC  9(004).
           03  WRK-SEQ-OPER            PIC  9(009).


       01  WRK-MSG01                   PIC  X(079)  VALUE
            'NAO FORAM ENCONTRADOS DADOS PARA ESTE ARGUMENTO DE PESQUISA
      -     ' '.
      *
       01  WRK-193                     PIC S9(008)  COMP VALUE +193.
       01  WRK-201                     PIC S9(008)  COMP VALUE +201.
       01  WRK-225                     PIC S9(008)  COMP VALUE +225.
       01  WRK-49353                   PIC S9(008)  COMP VALUE +49353.
       01  WRK-49345                   PIC S9(008)  COMP VALUE +49345.
       01  WRK-49369                   PIC S9(008)  COMP VALUE +49369.
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
           '* AREA ROTINA DCOM4720 *'.
      *----------------------------------------------------------------*
      *
       01  4720E-ROTEADOR.
           03  4720E-HEADER.
               05  4720E-COD-RETORNO             PIC  X(004).
               05  4720E-MSG-RETORNO             PIC  X(079).
               05  4720E-RESTART                 PIC  9(005).
               05  4720E-FLAG                    PIC  X(001).
               05  4720E-START-COUNT             PIC  X(001).
               05  4720E-CFUNC-BDSCO             PIC  9(009).
               05  4720E-CTERM                   PIC  X(008).
           03  4720E-DADOS.
               10  4720E-FLAG-FILTRO             PIC  X(002).
               10  4720E-CBCO                    PIC  9(003).
               10  4720E-CAG-BCRIA               PIC  9(005).
               10  4720E-CAG-OPER                PIC  9(005).
               10  4720E-CCTA-BCRIA-CLI          PIC  9(013).
               10  4720E-CPRODT                  PIC  9(003).
               10  4720E-CSPROD-DESC-COML        PIC  9(003).
               10  4720E-DINIC-OPER-DESC         PIC  X(010).
               10  4720E-CELMTO-DESC-COML        PIC  9(003).
FS2511*        10  4720E-CCNPJ-CPF               PIC  9(009).
FS2511         10  4720E-CCNPJ-CPF               PIC  X(009).
FS2511*        10  4720E-CFLIAL-CNPJ             PIC  9(005).
FS2511         10  4720E-CFLIAL-CNPJ             PIC  X(004).
               10  4720E-CCTRL-CNPJ-CPF          PIC  9(002).
               10  4720E-CSIT-DESC-COML          PIC  9(003).
               10  4720E-CINDCD-EVNTO-WORKFLOW   PIC  9(003).
               10  4720E-CINDCD-SIT-FINAL-EVNTO  PIC  9(003).

       01  4720S-ROTEADOR.
           03  4720S-HEADER.
               05  4720S-COD-RETORNO      PIC  X(004).
               05  4720S-MSG-RETORNO      PIC  X(079).
               05  4720S-RESTART          PIC  9(005).
               05  4720S-FLAG             PIC  X(001).
               05  4720S-QTDE-RETORNADA   PIC  9(003).
               05  4720S-COUNT            PIC  9(009).

           03  4720S-DADOS.
               05  4720S-TABELA-SAIDA     OCCURS 10  TIMES.
                   10  4720S-DANO-OPER-DESC        PIC  9(004).
                   10  4720S-NSEQ-OPER-DESC        PIC  9(009).
                   10  4720S-IPSSOA-DESC-COML      PIC  X(060).
                   10  4720S-CBCO                  PIC  9(003).
                   10  4720S-CAG-BCRIA             PIC  9(005).
                   10  4720S-CAG-BCRIA-DESC        PIC  X(040).
                   10  4720S-CCTA-BCRIA-CLI        PIC  9(013).
                   10  4720S-CBCO-OPER-OPER        PIC  9(003).
                   10  4720S-CAG-OPER-OPER         PIC  9(005).
FS2511*            10  4720S-CCNPJ-CPF             PIC  9(009).
FS2511             10  4720S-CCNPJ-CPF             PIC  X(009).
FS2511*            10  4720S-CFLIAL-CNPJ           PIC  9(005).
FS2511             10  4720S-CFLIAL-CNPJ           PIC  X(004).
                   10  4720S-CCTRL-CNPJ-CPF        PIC  9(002).
                   10  4720S-CSIT-DESC-COML        PIC  9(003).
                   10  4720S-IRSUMO-SIT-DESC       PIC  X(015).
                   10  4720S-CSGL-SIT-DESC         PIC  X(006).
                   10  4720S-CPRODT                PIC  9(003).
                   10  4720S-IABREV-PRODT          PIC  X(012).
                   10  4720S-CSGL-PRODT-DESC       PIC  X(006).
                   10  4720S-CSPROD-DESC-COML      PIC  9(003).
                   10  4720S-CTPO-DESC-COML        PIC  9(003).
                   10  4720S-IRSUMO-SPROD-DESC     PIC  X(010).
                   10  4720S-CSGL-SPROD-DESC       PIC  X(006).
                   10  4720S-VOPER-DESC-COML       PIC  9(015)V9(2).
                   10  4720S-DINIC-OPER-DESC       PIC  X(010).
                   10  4720S-DVCTO-FNAL-OPER       PIC  X(010).
                   10  4720S-CMEIO-ENTRD-DESC      PIC  9(03).
                   10  4720S-IMEIO-ENTRD-DESC      PIC  X(25).
                   10  4720S-IRSUMO-MEIO-ENTRD     PIC  X(15).
                   10  4720S-CSGL-MEIO-ENTRD       PIC  X(06).
                   10  4720S-CINDCD-LIBRC-ANTCP    PIC  X(01).
                   10  4720S-PARECER-FORML         PIC  X(01).
                   10  4720S-VLIQ-OPER-DESC        PIC  9(015)V9(2).

      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ROTINA DCOM5335 *'.
      *----------------------------------------------------------------*

       01  5335-ENTRADA.
           05 5335E-COD-RETORNO        PIC  X(004)         VALUE SPACES.
           05 5335E-MSG-RETORNO        PIC  X(079)         VALUE SPACES.
           05 5335E-RESTART            PIC  9(005)         VALUE ZEROS.
           05 5335E-FLAG               PIC  9(001)         VALUE ZEROS.
           05 5335E-CFUNC-BDSCO        PIC  9(009)         VALUE ZEROS.
           05 5335E-CTERM              PIC  X(008)         VALUE SPACES.
           05 5335E-OPERACAO.
              10 5335E-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
              10 5335E-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.
           05 5335E-NPCELA-DESC-COML   PIC  9(005)         VALUE ZEROS.
           05 5335E-CELMTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-CEVNTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-ACAO               PIC  X(001)         VALUE SPACES.

       01  5335-SAIDA.
           05 5335S-COD-RETORNO        PIC  9(004)         VALUE ZEROS.
           05 5335S-MSG-RETORNO        PIC  X(079)         VALUE SPACES.

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
FS2511*        10  COMU-CNPJCPF        PIC  9(009)         VALUE ZEROS.
FS2511         10  COMU-CNPJCPF        PIC  X(09)          VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-FILIAL         PIC  X(04)          VALUE SPACES.
               10  COMU-CONTROLE       PIC  9(002)         VALUE ZEROS.
               10  COMU-BANCO          PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)         VALUE ZEROS.
               10  COMU-OPERACAO       PIC  9(013)         VALUE ZEROS.
               10  FILLER REDEFINES    COMU-OPERACAO.
                   15 COMU-DANO        PIC  9(004).
                   15 COMU-NSEQ        PIC  9(009).
               10  COMU-CONTRATO-LIM   PIC  9(009)         VALUE ZEROS.
               10  COMU-PAGINA1        PIC  9(004)         VALUE ZEROS.
               10  COMU-OCCUR          PIC  9(008)         VALUE ZEROS.
           05  COMU-MENSAGEM           PIC  X(079)         VALUE SPACES.
           05  COMU-ACAO               PIC  X(001)         VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA INPUT DCOMECTM **'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-ECTM.
           03  FILLER                  PIC X(018)  VALUE SPACES.
           03  WRK-INP-PFK-ECTM        PIC X(001)  VALUE SPACES.
           03  WRK-INP-DADOS-ECTM.
               05  WRK-INP-SENHAS-ECTM PIC  X(037) VALUE SPACES.
               05  WRK-INP-COMANDO-ECTM   PIC  X(068) VALUE SPACES.
               05  WRK-INP-ESCOND-ECTM.
                   07  WRK-INP-OPCAO-ECTM PIC  9(001) VALUE ZEROS.
                   07  WRK-INP-CPAG-ECTM  PIC  9(003) VALUE ZEROS.
                   07  WRK-INP-QTIP-ECTM  PIC  9(001) VALUE ZEROS.
                   07  WRK-INP-FIMP-ECTM  PIC  X(001) VALUE SPACES.
                   07  WRK-INP-VAGO-ECTM  PIC  X(024) VALUE SPACES.
               05  WRK-INP-TIMEST-ECTM    PIC  X(026) VALUE SPACES.
               05  WRK-INP-PAG-ECTM.
                   07  WRK-INP-PAG-ECTM-N PIC  9(003) VALUE ZEROS.
               05  WRK-INP-AGENCIA-ECTM.
                   07  WRK-INP-AGENCIA-NUM    PIC  9(005) VALUE ZEROS.
               05  WRK-INP-NMAGEN-ECTM    PIC  X(040) VALUE SPACES.
               05  WRK-INP-TAB-ECTM   OCCURS   5   TIMES.
                   07  WRK-INP-SEL-ECTM   PIC  X(001).
                   07  WRK-INP-OPER-ECTM  PIC  X(013).
                   07  WRK-INP-CNPJ-ECTM  PIC  X(017).
                   07  WRK-INP-NOME-ECTM  PIC  X(030).
                   07  WRK-INP-SITU-ECTM  PIC  X(013).
                   07  WRK-INP-PROD-ECTM  PIC  X(019).
                   07  WRK-INP-SUBP-ECTM  PIC  X(019).
               05  WRK-INP-MSGEM-ECTM     PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA OUTPUT DCOMECTM *'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-ECTM.
           03  WRK-OUT-LL-ECTM         PIC S9(004)  COMP VALUE +0000.
           03  WRK-OUT-ZZ-ECTM         PIC S9(004)  COMP VALUE ZEROS.
           03  WRK-OUT-DADOS-ECTM.
               05  WRK-OUT-SENHAS-ECTM  PIC  X(037) VALUE SPACES.
               05  WRK-OUT-COMANDO-ECTM PIC  X(068) VALUE SPACES.
               05  WRK-OUT-ESCOND-ECTM.
                   07  WRK-OUT-OPCAO-ECTM PIC  9(001) VALUE ZEROS.
                   07  WRK-OUT-CPAG-ECTM  PIC  9(003) VALUE ZEROS.
                   07  WRK-OUT-QTIP-ECTM  PIC  9(001) VALUE ZEROS.
                   07  WRK-OUT-FIMP-ECTM  PIC  X(001) VALUE SPACES.
                   07  WRK-OUT-VAGO-ECTM  PIC  X(024) VALUE SPACES.
               05  WRK-OUT-TIMEST-ECTM     PIC  X(026) VALUE SPACES.
               05  WRK-OUT-PAG-ECTM.
                   07  WRK-OUT-PAG-ECTM-N PIC 9(03) VALUE ZEROS.
               05  WRK-OUT-AGENCIA-ECTM PIC  X(005) VALUE ZEROS.
               05  WRK-OUT-AGENCIA-NUM  REDEFINES  WRK-OUT-AGENCIA-ECTM
                                        PIC  9(005).
               05  WRK-OUT-NMAGEN-ECTM  PIC  X(040) VALUE SPACES.
               05  WRK-OUT-TAB-ECTM   OCCURS   5   TIMES.
                   07  WRK-OUT-SEL-ATTR   PIC S9(004) COMP.
                   07  WRK-OUT-SEL-ECTM   PIC  X(001).
                   07  WRK-OUT-OPER-ECTM  PIC  X(013).
                   07  WRK-OUT-CNPJ-ECTM  PIC  X(017).
                   07  WRK-OUT-NOME-ECTM  PIC  X(030).
                   07  WRK-OUT-SITU-ECTM  PIC  X(013).
                   07  WRK-OUT-PROD-ECTM  PIC  X(019).
                   07  WRK-OUT-SUBP-ECTM  PIC  X(019).
               05  WRK-OUT-MSGEM-ECTM     PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA BRAD0660 (ECTM) *'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMECTM.
           05  WRK-660-LL-AREA-ECTM        PIC  9(004) COMP VALUE  0000.
           05  WRK-660-LL-MENSAGEM-ECTM    PIC  9(004) COMP VALUE  0000.
           05  WRK-660-SENHAS-ECTM         PIC  9(004) COMP VALUE  0037.
           05  WRK-660-ESCOND-ECTM         PIC  9(004) COMP VALUE  0030.
           05  WRK-660-TIM-ECTM            PIC  9(004) COMP VALUE  0026.
           05  WRK-660-PAG-ECTM            PIC  9(004) COMP VALUE  0003.
           05  WRK-660-COMANDO-ECTM        PIC  9(004) COMP VALUE  1068.
           05  WRK-660-AGENCIA-ECTM        PIC  9(004) COMP VALUE  0005.
           05  WRK-660-NMAGEN-ECTM         PIC  9(004) COMP VALUE  0040.
           05  WRK-600-LISTA           OCCURS  05.
               10 WRK-660-SELEC-ECTM       PIC  9(004) COMP VALUE  2003.
               10 WRK-660-OPER-ECTM        PIC  9(004) COMP VALUE  0013.
               10 WRK-660-CNPJ-ECTM        PIC  9(004) COMP VALUE  0017.
               10 WRK-660-NOME-ECTM        PIC  9(004) COMP VALUE  0030.
               10 WRK-660-SITU-ECTM        PIC  9(004) COMP VALUE  0013.
               10 WRK-660-PROD-ECTM        PIC  9(004) COMP VALUE  0019.
               10 WRK-660-SUBP-ECTM        PIC  9(004) COMP VALUE  0019.
           05  WRK-660-MSGEM-ECTM          PIC  9(004) COMP VALUE  0079.
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

           MOVE    SPACES              TO          WRK-OUT-DADOS-ECTM
                                                   WRK-INCONSIS.

           INITIALIZE                              4720E-ROTEADOR
                                                   4720S-ROTEADOR.

           MOVE    'DCOMECTM'          TO          WRK-TELA.

           INITIALIZE          COMU-AREA.

           IF      WRK-TRANSACAO       NOT EQUAL  'DCOM0503'
                   PERFORM             0200-00-RECEBER-CONTROLE
           ELSE
                   PERFORM             0500-00-PROCESSAR-DCOM0503
           END-IF.

           IF      WRK-TELA            EQUAL      'DCOMECTM'
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
           MOVE    COMU-TPOPCAO        TO        WRK-INP-OPCAO-ECTM
                                                 WRK-OUT-OPCAO-ECTM.
           MOVE    COMU-TIMESTAMP      TO        WRK-OUT-TIMEST-ECTM
                                                 WRK-INP-TIMEST-ECTM.

           IF  WRK-TRANSACAO           EQUAL 'DCOM0502'
               MOVE    1               TO  WRK-INP-PAG-ECTM-N
           ELSE
               IF  COMU-PAGINA1            GREATER 1
                   COMPUTE 4720E-RESTART   = (COMU-PAGINA1 - 1) * 5
                   MOVE    COMU-PAGINA1    TO  WRK-INP-PAG-ECTM-N
                                               WRK-OUT-PAG-ECTM-N
               ELSE
                   MOVE    1               TO  WRK-INP-PAG-ECTM-N
                                               WRK-OUT-PAG-ECTM-N
               END-IF
           END-IF.

           MOVE    COMU-AGENCIA        TO          WRK-INP-AGENCIA-NUM
                                                   WRK-OUT-AGENCIA-NUM.

           PERFORM 0230-00-CH-DCOM6416.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0200-99-FIM
           END-IF.

           PERFORM 0210-00-CH-DCOM4720.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0200-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           MOVE    WRK-INP-ESCOND-ECTM TO          WRK-OUT-ESCOND-ECTM.
           MOVE    WRK-INP-PAG-ECTM-N  TO          WRK-OUT-PAG-ECTM-N.

           IF  WRK-TRANSACAO               EQUAL     'DCOM0505' OR
                                                     'DCOM0506' OR
                                                     'DCOM0507'
               IF  COMU-MENSAGEM           NOT EQUAL  SPACES
                   MOVE    COMU-MENSAGEM          TO  WRK-OUT-MSGEM-ECTM
               ELSE
                   MOVE    SG-MENSAGEM(214)       TO  WRK-OUT-MSGEM-ECTM
               END-IF
           ELSE
               MOVE    SG-MENSAGEM(214)    TO  WRK-OUT-MSGEM-ECTM
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA DCOM4720                *
      ******************************************************************
       0210-00-CH-DCOM4720             SECTION.
      *----------------------------------------------------------------*


           EVALUATE  WRK-INP-OPCAO-ECTM
              WHEN 1
                 MOVE 'IE'                   TO 4720E-FLAG-FILTRO
              WHEN 2
                 MOVE 'EE'                   TO 4720E-FLAG-FILTRO
              WHEN 3
                 MOVE 'CE'                   TO 4720E-FLAG-FILTRO
           END-EVALUATE

           MOVE 237                    TO 4720E-CBCO.
           MOVE WRK-INP-AGENCIA-NUM    TO 4720E-CAG-BCRIA
                                          4720E-CAG-OPER.
           MOVE WRK-COD-USER           TO 4720E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4720E-CTERM
           MOVE   'DCOM4720'           TO          WRK-MODULO.

           CALL    WRK-MODULO          USING       4720E-ROTEADOR
                                                   4720S-ROTEADOR
                                                   ERRO-AREA
                                                   WRK-SQLCA.

           EVALUATE  4720S-COD-RETORNO

             WHEN '0000'
             WHEN '0002'
                   IF   4720S-DANO-OPER-DESC(6)    EQUAL ZEROS AND
                        4720S-FLAG                 EQUAL 'S'
                        MOVE 4720S-FLAG       TO   WRK-INP-FIMP-ECTM
                                                   WRK-OUT-FIMP-ECTM
                   ELSE
                        MOVE SPACES           TO   WRK-INP-FIMP-ECTM
                                                   WRK-OUT-FIMP-ECTM
                   END-IF

             WHEN '0003'

                   IF  COMU-PAGINA1            EQUAL  1 AND
                       COMU-OCCUR              EQUAL  1 AND
                       COMU-ACAO               EQUAL 'S'
                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-ECTM
                   ELSE
                       MOVE SG-MENSAGEM(213)   TO  COMU-MENSAGEM
                   END-IF
                   PERFORM             0520-00-RETORNAR-CHAMADOR

             WHEN '0099'
                   MOVE  'DB2'         TO          ERR-TIPO-ACESSO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO

             WHEN  OTHER

                   IF  COMU-PAGINA1            EQUAL  1 AND
                       COMU-OCCUR              EQUAL  1 AND
                       COMU-ACAO               EQUAL 'S'
                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-ECTM
                   ELSE
                       MOVE 4720S-MSG-RETORNO  TO  COMU-MENSAGEM
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
                                            TO WRK-INP-NMAGEN-ECTM
                                               WRK-OUT-NMAGEN-ECTM

               WHEN '0099'
                    MOVE 'DB2'               TO ERR-TIPO-ACESSO
                    PERFORM 9995-00-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE WRK-MSG01      TO COMU-MENSAGEM
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

           PERFORM VARYING WIND        FROM 1 BY 1
                     UNTIL WIND        GREATER 5

             IF   4720S-DANO-OPER-DESC(WIND) GREATER ZEROS
                  MOVE  WRK-193       TO  WRK-OUT-SEL-ATTR(WIND)
                  MOVE 4720S-CCNPJ-CPF(WIND)
                                      TO WRK-OUT-CNPJ-ECTM(WIND)(1:9)
                  MOVE '/'            TO WRK-OUT-CNPJ-ECTM(WIND)(10:1)
                  MOVE 4720S-CFLIAL-CNPJ(WIND)(2:4)
                                      TO WRK-OUT-CNPJ-ECTM(WIND)(11:4)
                  MOVE '-'            TO WRK-OUT-CNPJ-ECTM(WIND)(15:1)
                  MOVE 4720S-CCTRL-CNPJ-CPF(WIND)
                                      TO WRK-OUT-CNPJ-ECTM(WIND)(16:2)
                  MOVE 4720S-DANO-OPER-DESC(WIND)
                                      TO  WRK-ANO-OPER
                  MOVE 4720S-NSEQ-OPER-DESC(WIND)
                                      TO  WRK-SEQ-OPER
                  MOVE WRK-OPERACAO   TO  WRK-OUT-OPER-ECTM(WIND)

                  MOVE 4720S-CPRODT(WIND)
                                      TO  WRK-COD-PROD
                  MOVE 4720S-IABREV-PRODT(WIND)
                                      TO  WRK-DESC-PROD
                  MOVE '-'            TO  WRK-BARRA-PROD
                  MOVE WRK-PRODUTO    TO  WRK-OUT-PROD-ECTM(WIND)

                  MOVE 4720S-CSPROD-DESC-COML(WIND)
                                      TO  WRK-COD-SUBP
                  MOVE 4720S-IRSUMO-SPROD-DESC(WIND)
                                      TO  WRK-DESC-SUBP
                  MOVE '-'            TO  WRK-BARRA-SUBP
                  MOVE WRK-SUBPROD    TO  WRK-OUT-SUBP-ECTM(WIND)

                  MOVE 4720S-IPSSOA-DESC-COML(WIND)
                                      TO  WRK-OUT-NOME-ECTM(WIND)
                  MOVE 4720S-IRSUMO-SIT-DESC(WIND)
                                      TO  WRK-OUT-SITU-ECTM(WIND)
             ELSE
                  MOVE  WRK-225       TO  WRK-OUT-SEL-ATTR(WIND)
                  MOVE  SPACES        TO  WRK-OUT-OPER-ECTM(WIND)
                                          WRK-OUT-CNPJ-ECTM(WIND)
                                          WRK-OUT-NOME-ECTM(WIND)
                                          WRK-OUT-SITU-ECTM(WIND)
                                          WRK-OUT-PROD-ECTM(WIND)
                                          WRK-OUT-SUBP-ECTM(WIND)
             END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RECEBIMENTO DE MENSAGEM DO PROGRAMA      *
      ******************************************************************
       0500-00-PROCESSAR-DCOM0503      SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM        TO          WRK-INPUT-ECTM.
           PERFORM 0510-00-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-ECTM     EQUAL 'H' OR
                   (WRK-INP-PFK-ECTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ECTM EQUAL 'PFK01')
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-INP-MSGEM-ECTM     TO WRK-OUT-MSGEM-ECTM
                    IF  WRK-OUT-OPER-ECTM(1)    NOT EQUAL ZEROS
                        MOVE WRK-49345 TO WRK-OUT-SEL-ATTR(1)
                    END-IF

               WHEN WRK-INP-PFK-ECTM     EQUAL '3' OR
                   (WRK-INP-PFK-ECTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ECTM EQUAL 'PFK03')
                    PERFORM 0520-00-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-ECTM     EQUAL 'X' OR
                   (WRK-INP-PFK-ECTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ECTM EQUAL 'PFK05')
                    PERFORM 0530-00-MENU-ROTINAS

               WHEN WRK-INP-PFK-ECTM     EQUAL 'V' OR
                   (WRK-INP-PFK-ECTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ECTM EQUAL 'PFK10')
                    PERFORM 0540-00-MENU-DCOM

               WHEN WRK-INP-PFK-ECTM     EQUAL '7' OR
                   (WRK-INP-PFK-ECTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ECTM EQUAL 'PFK07')
                    PERFORM 0550-00-TELA-ANTERIOR

               WHEN WRK-INP-PFK-ECTM     EQUAL '8' OR
                   (WRK-INP-PFK-ECTM     EQUAL '.' AND
                    WRK-INP-COMANDO-ECTM EQUAL 'PFK08')
                    PERFORM 0560-00-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-ECTM          EQUAL '.' AND
                     WRK-INP-COMANDO-ECTM      EQUAL SPACES)
                    PERFORM 0511-00-DEVOLVER-OCORR
                    PERFORM 0570-00-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-49345         TO WRK-OUT-SEL-ATTR(1)
                    MOVE 'PF INVALIDA'     TO WRK-OUT-MSGEM-ECTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0510-00-DEVOLVER-TELA           SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-INP-ESCOND-ECTM   TO        WRK-OUT-ESCOND-ECTM.
           MOVE    WRK-INP-PAG-ECTM      TO        WRK-OUT-PAG-ECTM.
           MOVE    WRK-INP-AGENCIA-ECTM  TO        WRK-OUT-AGENCIA-ECTM.
           MOVE    WRK-INP-NMAGEN-ECTM   TO        WRK-OUT-NMAGEN-ECTM.
           MOVE    WRK-INP-TIMEST-ECTM   TO        WRK-OUT-TIMEST-ECTM.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0511-00-DEVOLVER-OCORR          SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WIND     FROM 1 BY 1
                     UNTIL WIND     GREATER 5

               IF  WRK-INP-OPER-ECTM(WIND)    NOT EQUAL SPACES
                   MOVE  WRK-INP-SEL-ECTM(WIND)
                     TO  WRK-OUT-SEL-ECTM(WIND)
                   MOVE  WRK-INP-OPER-ECTM(WIND)
                     TO  WRK-OUT-OPER-ECTM(WIND)
                   MOVE  WRK-INP-CNPJ-ECTM(WIND)
                     TO  WRK-OUT-CNPJ-ECTM(WIND)
                   MOVE  WRK-INP-NOME-ECTM(WIND)
                     TO  WRK-OUT-NOME-ECTM(WIND)
                   MOVE  WRK-INP-SITU-ECTM(WIND)
                     TO  WRK-OUT-SITU-ECTM(WIND)
                   MOVE  WRK-INP-PROD-ECTM(WIND)
                     TO  WRK-OUT-PROD-ECTM(WIND)
                   MOVE  WRK-INP-SUBP-ECTM(WIND)
                     TO  WRK-OUT-SUBP-ECTM(WIND)
               ELSE
                   MOVE  WRK-225       TO  WRK-OUT-SEL-ATTR(WIND)
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

           MOVE    WRK-INP-TIMEST-ECTM   TO   COMU-TIMESTAMP.
           MOVE    237                   TO   COMU-BANCO
           MOVE    WRK-INP-AGENCIA-NUM   TO   COMU-AGENCIA
           MOVE   'DCOM0503'             TO   COMU-TRANSACAO
           MOVE    LENGTH                OF   COMU-AREA
                                         TO   COMU-LL.
           MOVE   'DCOM0502'             TO   WRK-TELA.
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
           MOVE   'DCOM0503'           TO          COMU-TRANSACAO
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
           MOVE   'DCOM0503'           TO          COMU-TRANSACAO
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

           IF  WRK-INP-PAG-ECTM-N      EQUAL 1
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MSGEM-ECTM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0550-99-FIM
           END-IF.

           COMPUTE 4720E-RESTART      = (WRK-INP-PAG-ECTM-N - 2) * 5

           PERFORM 0210-00-CH-DCOM4720.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0550-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           MOVE SG-MENSAGEM(01)        TO WRK-OUT-MSGEM-ECTM
           COMPUTE WRK-OUT-PAG-ECTM-N  = WRK-INP-PAG-ECTM-N - 1.

      *----------------------------------------------------------------*
       0550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO AVANCO DE TELA NA PAGINACAO              *
      ******************************************************************
       0560-00-TELA-POSTERIOR          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMP-ECTM       EQUAL 'S'
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MSGEM-ECTM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0560-99-FIM
           END-IF.

           COMPUTE 4720E-RESTART       = (WRK-INP-PAG-ECTM-N * 5)

           PERFORM 0210-00-CH-DCOM4720.
           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0560-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           IF   WRK-INP-FIMP-ECTM       EQUAL 'S'
                MOVE SG-MENSAGEM(17)    TO WRK-OUT-MSGEM-ECTM
           ELSE
                MOVE SG-MENSAGEM(01)    TO WRK-OUT-MSGEM-ECTM
           END-IF

           COMPUTE WRK-OUT-PAG-ECTM-N   = WRK-INP-PAG-ECTM-N + 1.

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

           PERFORM VARYING WIND     FROM 1 BY 1
                     UNTIL WIND     GREATER 5

             IF    WRK-INP-SEL-ECTM(WIND)
                                       NOT EQUAL   SPACES AND LOW-VALUES

                   MOVE  WRK-49353     TO  WRK-OUT-SEL-ATTR(WIND)
               IF  WRK-INP-SEL-ECTM(WIND)  EQUAL  'X'
                   ADD   1             TO  WRK-COUNT
                   MOVE  WIND          TO  WINDT
               ELSE
                   MOVE '*'            TO  WRK-INCONSIS
                   MOVE  WRK-49369     TO  WRK-OUT-SEL-ATTR(WIND)
               END-IF
             END-IF

           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL '*'
               MOVE SG-MENSAGEM(086)   TO WRK-OUT-MSGEM-ECTM
           ELSE
               IF  WRK-COUNT           EQUAL ZEROS
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE SG-MENSAGEM(214)
                                       TO WRK-OUT-MSGEM-ECTM
               ELSE
                   IF  WRK-COUNT       GREATER 1
                       MOVE '*'        TO WRK-INCONSIS
                       MOVE SG-MENSAGEM(215)
                                       TO WRK-OUT-MSGEM-ECTM
                   ELSE
                       PERFORM 0571-50-ACESSAR-DCOM5335
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       0571-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      *    VERIFICAR PERMICAO DE ESTORNO DE OPERACAO                   *
      ******************************************************************
       0571-50-ACESSAR-DCOM5335        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPER-ECTM(WINDT) TO 5335E-OPERACAO.
           MOVE 006                      TO 5335E-CELMTO-DESC-COML.
           MOVE 009                      TO 5335E-CEVNTO-DESC-COML.

           EVALUATE WRK-INP-OPCAO-ECTM
               WHEN 1
                    MOVE 'I'             TO 5335E-ACAO
               WHEN 2
                    MOVE 'E'             TO 5335E-ACAO
               WHEN 3
                    GO TO 0571-50-99-FIM
           END-EVALUATE.

           MOVE 'DCOM5335'               TO WRK-MODULO.

           CALL WRK-MODULO               USING 5335-ENTRADA
                                               5335-SAIDA
                                               ERRO-AREA
                                               WRK-SQLCA.

           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'         TO ERR-TIPO-ACESSO
                     PERFORM 9995-00-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                        TO WRK-OUT-MSGEM-ECTM
                     MOVE '*'           TO WRK-INCONSIS
                     MOVE WRK-49369     TO WRK-OUT-SEL-ATTR(WINDT)
           END-EVALUATE.

      *----------------------------------------------------------------*
       0571-50-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      *    SECTION TRATAMENTO CONTROLE TRANSFERENCIA OUTROS PROGRAMAS  *
      ******************************************************************
       0572-00-TRANSFERIR-CONTROLE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE    WRK-INP-OPCAO-ECTM          TO  COMU-TPOPCAO
           MOVE    WRK-INP-TIMEST-ECTM         TO  COMU-TIMESTAMP
           MOVE   'DCOM0503'                   TO  COMU-TRANSACAO

           MOVE    LENGTH  OF  COMU-AREA
                                               TO  COMU-LL
           MOVE    237                         TO  COMU-BANCO
           MOVE    WRK-INP-AGENCIA-NUM         TO  COMU-AGENCIA
           MOVE    WRK-INP-PAG-ECTM-N          TO  COMU-PAGINA1

           IF      WRK-INP-PAG-ECTM-N          EQUAL 1  AND
                   WRK-INP-OPER-ECTM(2)        EQUAL SPACES OR
                                                     LOW-VALUES
                   MOVE 1                      TO  COMU-OCCUR
           END-IF

           MOVE    WRK-INP-OPER-ECTM(WINDT) TO  COMU-OPERACAO.

           EVALUATE  WRK-INP-OPCAO-ECTM
              WHEN  1
                    MOVE 'DCOM0505'     TO          WRK-TELA
              WHEN  2
                    MOVE 'DCOM0506'     TO          WRK-TELA
              WHEN  3
                    MOVE 'DCOM0507'     TO          WRK-TELA
              WHEN  OTHER
                    MOVE 'OPCAO INVALIDA'
                                       TO          WRK-OUT-MSGEM-ECTM
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

           MOVE LENGTH                 OF  WRK-OUTPUT-ECTM
                                       TO  WRK-660-LL-MENSAGEM-ECTM
                                           WRK-OUT-LL-ECTM

           MOVE LENGTH                 OF  WRK-660-DCOMECTM
                                       TO  WRK-660-LL-AREA-ECTM

           CALL   'BRAD0660'           USING       WRK-OUTPUT-ECTM
                                                   WRK-660-DCOMECTM.

           IF      RETURN-CODE         NOT EQUAL   ZEROS
                   MOVE 'APL'          TO          ERR-TIPO-ACESSO
                   MOVE  RETURN-CODE   TO          WRK-RETURN-CODE
                   MOVE  20            TO          WRK-LOCAL-ERRO
                   MOVE  WRK-ERRO-BRAD0660 TO      ERR-TEXTO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE    WRK-ISRT            TO          WRK-FUNCAO.
           MOVE    WRK-OUTPUT-ECTM     TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA BRAD7100 (ABENDS)       *
      ******************************************************************
       9995-00-PROCESSAR-ROTINA-ERRO   SECTION.
      *----------------------------------------------------------------*

           IF      ERR-MODULO          NOT EQUAL   SPACES AND LOW-VALUES
                   MOVE 'DCOM0503'     TO          ERR-PGM
           ELSE
                   MOVE 'SENHAS02'     TO          ERR-PGM
                   MOVE 'DCOM0503'     TO          ERR-MODULO
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
