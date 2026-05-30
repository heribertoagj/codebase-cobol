      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE CONTRATO LIMITE - LAYOUT SAIDA *
      * COPYBOOK   : DCOMWAJS - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AJL - LISTA DETALHADA DE CONTRATO LIMITE     *
      * FLUXO      : DCOMIAA9                                          *
      * DATA       : 03/08/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWAJS-QTOT-CONTRATOS                                        *
      *                       = IDENTIFICA A QUANTIDADE TOTAL DE CON-  *
      *                         TRATOS EXISTES PARA O FILTRO PESQUISA  *
      * DCOMWAJS-QTDE-CONTRATOS                                        *
      *                       = IDENTIFICA A QUANTIDADE DE CONTRATOS   *
      *                         LIMITE LISTADO PARA A CONSULTA         *
      **                                                              **
      ** ------------------------------------------------------------ **
      * ---> DADOS BASICO DO CONTRATO LIMITE - OCORRE AT� 5 VEZES <--- *
      * ---> ==================================================== <--- *
      **                                                              **
      * DCOMWAJS-CCONTR-LIM-DESC                                       *
      *                       = CODIGO DO CONTRATO LIMITE              *
      * DCOMWAJS-CVRSAO-CONTR-LIM                                      *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      * DCOMWAJS-CSIT-DESC-COML                                        *
      *                       = CODIGO DA SITUACAO DO CONTRATO LIMITE  *
      * DCOMWAJS-IRSUMO-SIT-DESC                                       *
      *                       = DESCRICAO DA SITUACAO DO CONTRATO LIMIT*
      * DCOMWAJS-CPRODT-SERVC-OPER                                     *
      *                       = CODIGO DO PRODUTO DCOM CADASTRADO CDPS *
      * DCOMWAJS-CPRODT       = CODIGO DO PRODUTO DCOM                 *
      * DCOMWAJS-IPRODT       = DESCRICAO DO PRODUTO DCOM              *
      * DCOMWAJS-CSPROD-DESC-COML                                      *
      *                       = CODIGO DO SUBPRODUTO DCOM              *
      * DCOMWAJS-IRSUMO-SPROD-DESC                                     *
      *                       = DESCRICAO DO SUBPRODUTO DCOM           *
      * DCOMWAJS-CCLUB        = CLUB DO CEDENTE/PROTONENDO DO CONTRATO *
      * DCOMWAJS-CCNPJ-CPF    = NUMERO DO CNPJ/CPF DO CLIENTE          *
      * DCOMWAJS-CFLIAL-CNPJ  = NUMERO DA FILIAL DO CNPJ DO CLIENTE    *
      * DCOMWAJS-CCTRL-CNPJ-CPF                                        *
      *                       = CODIGO DO CONTROLE DO CNPJ/CPF CLIENTE *
      * DCOMWAJS-CAG-BCRIA    = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWAJS-CCTA-BCRIA-CLI                                        *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      * DCOMWAJS-IPSSOA-DESC-COML                                      *
      *                       = NOME DO CEDENTE/PROPONENTE DO CONTRATO *
      * DCOMWAJS-CINDCD-MODLD-DESC                                     *
      *                       = INDICADOR DA MODALIDADE DE DESCONTO    *
      * DCOMWAJS-CBCO         = CODIGO DO BANCO [237 - BRADESCO]       *
      * DCOMWAJS-CAG-BCRIA    = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWAJS-CCTA-BCRIA-CLI                                        *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      * DCOMWAJS-CBCO-OPER-CONTR                                       *
      *                       = CODIGO DO BANCO OPERADOR               *
      * DCOMWAJS-CAG-OPER-CONTR                                        *
      *                       = CODIGO DA AGENCIA OPERADORA            *
      * DCOMWAJS-DINCL-CONTR-LIM                                       *
      *                       = DATA DA INCLUSAO DO CONTRATO LIMITE    *
      * DCOMWAJS-DINIC-VGCIA-CONTR                                     *
      *                       = DATA DE INICIO DA VIGENCIA DO CONTRATO *
      * DCOMWAJS-DVCTO-CONTR-LIM                                       *
      *                       = DATA DE VENCTO DA VIGENCIA DO CONTRATO *
      * DCOMWAJS-TCONTR-LIM-DESC                                       *
      *                       = PRAZO DO CONTRATO LIMITE EM DIAS       *
      * DCOMWAJS-CINDCD-PRZ-CONTR                                      *
      *                       = INDICADOR DO PRAZO [DC                 *
      *                         ["DC"=DIAS CORRIDOS / "DU"=DIAS UTEIS] *
      * DCOMWAJS-VCONTR-LIM-DESC                                       *
      *                       = VALOR DO CONTRATO LIMITE               *
      * DCOMWAJS-VUTILZ-CONTR-LIM                                      *
      *                       = VALOR UTILIZADO DO CONTRATO LIMITE     *
      * DCOMWAJS-VDISPN-CONTR-LIM                                      *
      *                       = VALOR DISPONIVEL DO CONTRATO LIMITE    *
      * DCOMWAJS-CINDCD-CONTR-ROTVO                                    *
      *                       = INDICADOR DE CONTRATO ROTATIVO [S= SIM *
      *                         ["S" = SIM / "N' = NAO]                *
      * DCOMWAJS-CINDCD-RENOV-AUTOM                                    *
      *                       = INDICADOR DE RENOVACAO AUTOMATICA      *
      *                         ["S" = SIM / "N' = NAO]                *
      * DCOMWAJS-CINDCD-RTEIO-BNEFC                                    *
      *                       = INDICADOR DE RATEIO BENEFICIARIO       *
      *                         ["S" = SIM / "N' = NAO]                *
      * DCOMWAJS-CINDCD-LIBRC-CONTR                                    *
      *                       = INDICADOR LIBERACAO                    *
      *                         ["S" = SIM / "N' = NAO]                *
      * DCOMWAJS-CINDCD-TARIF-ADTTO                                    *
      *                       = INDICADOR DE TARIFA ADITADA            *
      *                         ["S" = SIM / "N' = NAO]                *
      * DCOMWAJS-DINIC-PARMZ-CONTR                                     *
      *                       = DATA DE INICIO PARAMETRIZACAO(DCOMB039)*
      * DCOMWAJS-CTPO-COBR-TAC                                         *
      *                       = CODIGO DO TIPO DE COBRANCA DA TAC      *
      *                         [001= ISENTO DE COBRANCA DE TAC        *
      *                          002= VALOR DE TAC FIXO NO CONTRATO    *
      *                          003= PERCENTUAL SOBRE VALOR CONTRATO  *
      *                          004= TAC SUBPRODUTO]                  *
      * DCOMWAJS-ITPO-COBR-TAC                                         *
      *                       = DESCRICAO DO TIPO DE COBRANCA DA TAC   *
      * DCOMWAJS-CINDCD-TAC-SPROD                                      *
      *                       = IDENTIFICA SE TAC A SER UTILIZADA � A  *
      *                         PARAMETRIZADA NO SUBPRODUTO DCOM       *
      *                         ["S" = SIM / "N' = NAO]                *
      * DCOMWAJS-VTAC-CONTR-LIM                                        *
      *                       = VALOR DE TAC                           *
      * DCOMWAJS-DTAC-CONTR-LIM                                        *
      *                       = DATA DA TAC                            *
      * DCOMWAJS-DRENOV-CONTR-LIM                                      *
      *                       = DATA DE RENOVACAO DO CONTRATO LIMITE   *
      * DCOMWAJS-CGARAN       = CODIGO DA GARANTIA DO CONTRATO LIMITE  *
      * DCOMWAJS-IGARAN       = DESCRICAO DA GARANTIA DO CONTRATO LIMIT*
      * DCOMWAJS-RPRCER-FORML-DESC                                     *
      *                       = DESCRICAO DO PARECER DA FORMALIZACAO   *
      **                                                              **
      ** ------------------------------------------------------------ **
      * DCOMWAJS-QTDE-LIMCRED = IDENTIFICA A QUANTIDADE DE CONSULTAS   *
      *                         DE LIMITE DE CREDITO PARA A CONSULTA   *
      **                                                              **
      * ---> LISTA CONSULTAS LIMITE DE CREDITO-OCORRE AT� 10 VEZES<--- *
      * ---> =====================================================<--- *
      **                                                              **
      * DCOMWAJS-NDOCTO-PARTC-LIM                                      *
      *                       = NUMERO CONSULTA CLIM                   *
      * DCOMWAJS-NSEQ-CONTR-LIM                                        *
      *                       = NUMERO DA SEQUENCIA DO CONTRATO CLIM   *
      * DCOMWAJS-DANO-BASE    = CONSULTA DE CREDITO(ANO BASE CONE)     *
      * DCOMWAJS-CDEPDC       = CONSULTA DE CREDITO(AGENCIA CONE)      *
      * DCOMWAJS-CSEQ-STUDO   = CONSULTA DE CREDITO(SEQUENCIA CONE)    *
      * DCOMWAJS-CCART-LIM-CREDT                                       *
      *                       = CONSULTA DE CREDITO(CARTEIRA CONE)     *
      * DCOMWAJS-VCONS-NEGOC  = CONSULTA DE CREDITO(VL CONSUL CONE/CLIM*
      **                                                              **
      ** ------------------------------------------------------------ **
      * --->              OPTIN - AUTORIZACAO DE DEBITO           <--- *
      * ---> =====================================================<--- *
      **                                                              **
      * DCOMWAJS-CINDCD-FORMA-LIQDC                                    *
      *                       = IDENTIFICA A FORMA DE LIQUIDEZ         *
      *                          ["D"= DEBITO EM CONTA / "B"= BOLETO]  *
      * DCOMWAJS-CINDCD-UTILZ-LIM                                      *
      *                       = IDENTIFICA A UTILIZACAO LIMITE CREDITO *
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWAJS-CINDCD-DEB-PCIAL                                      *
      *                       = IDENTIFICA DEBITO PARCIAL              *
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWAJS-CINDCD-TENTV-DEB                                      *
      *                       = IDENTIFICA TENTATIVA DEBITO(TEIMOSINHA)*
      *                          ["S"= SIM / "N" NAO/                  *
      **                                                              **
      ** ------------------------------------------------------------ **
      * DCOMWAJS-QTDE-TEST    = IDENTIFICA QUANTIDADE DE TESTEMUNHAS   *
      **                                                              **
      * --->             TESTEMUNHAS - OCORRE AT� 2 VEZES         <--- *
      * ---> =====================================================<--- *
      **                                                              **
      * DCOMWAJS-TEST-CCNPJ-CPF                                        *
      *                       = NUMERO DO CNPJ/CPF DA TESTEMUNHA       *
      * DCOMWAJS-TEST-CFLIAL-CNPJ                                      *
      *                       = NUMERO DA FILIAL DO CNPJ DA TESTEMUNHA *
      * DCOMWAJS-TEST-CCTRL-CNPJ-CPF                                   *
      *                       = CODIGO DO CONTROLE DO CNPJ/CPF TESTEM. *
      * DCOMWAJS-TEST-CDOCTO-ID                                        *
      *                       = CODIGO DO DOCUMENTO DA TESTEMUNHA      *
      * DCOMWAJS-TEST-IPSSOA-DESC                                      *
      *                       = NOME DA TESTEMUNHA                     *
      **                                                              **
      ** ------------------------------------------------------------ **
      * DCOMWAJS-QTDE-FIAD    = IDENTIFICA QUANTIDADE DE FIADORES      *
      **                                                              **
      * --->             FIADORES    - OCORRE AT� 8 VEZES         <--- *
      * ---> =====================================================<--- *
      **                                                              **
      * DCOMWAJS-FIAD-TPO-PSSOA                                        *
      *                       = IDENFITICADOR DO TIPO DE PESSOA        *
      * DCOMWAJS-FIAD-CCNPJ-CPF                                        *
      *                       = NUMERO DO CNPJ/CPF DO FIADOR           *
      * DCOMWAJS-FIAD-CFLIAL-CNPJ                                      *
      *                       = NUMERO DA FILIAL DO CNPJ DO FIADOR     *
      * DCOMWAJS-FIAD-CCTRL-CNPJ-CPF                                   *
      *                       = CODIGO DO CONTROLE DO CNPJ/CPF FIADOR  *
      * DCOMWAJS-FIAD-CDOCTO-ID                                        *
      *                       = CODIGO DO DOCUMENTO DO FIADOR          *
      * DCOMWAJS-FIAD-IPSSOA-DESC                                      *
      *                       = NOME DO FIADOR                         *
      * DCOMWAJS-FIAD-CBCO    = CODIGO DO BANCO DO FIADOR              *
      * DCOMWAJS-FIAD-CAG-BCRIAG                                       *
      *                       = CODIGO DA AGENCIA DO FIADOR            *
      * DCOMWAJS-FIAD-CCTA-BCRIA-CLI                                   *
      *                       = CODIGO DA CONTA DO FIADOR              *
      * DCOMWAJS-FIAD-ELOGDR  = ENDERECO DO FIADOR                     *
      * DCOMWAJS-FIAD-IBAIRRO = BAIRRO DO FIADOR                       *
      * DCOMWAJS-FIAD-CID-ELOGDR                                       *
      *                       = CIDADE DO FIADOR                       *
      * DCOMWAJS-FIAD-CSGL-UF = ESTADO DO FIADOR                       *
      * DCOMWAJS-FIAD-CCEP    = CEP DO FIADOR                          *
      * DCOMWAJS-FIAD-CCEP-COMPL                                       *
      *                       = COMPLEMENTO DO CEP DO FIADOR           *
      * DCOMWAJS-CINDCD-CONJG-AUTRZ                                    *
      *                       = IDENTIFICADOR DE CONJUGE AUTORIZADOR   *
      * DCOMWAJS-IPSSOA-CONJG = NOME DO CONJUGE DO FIADOR              *
      * DCOMWAJS-CCPF-CONJG   = NUMERO DO CNPJ/CPF DO CONJUGE FIADOR   *
      * DCOMWAJS-CCTRL-CPF-CONJG                                       *
      *                       = NUMERO DO CONTROLE CPF CONJUGE FIADOR  *
      * DCOMWAJS-CDOCTO-ID-CONJG                                       *
      *                       = NUMERO DO DOCUMENTO DO CONJUGE FIADOR  *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAJS-HEADER.
             10 DCOMWAJS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAJS'.
             10 DCOMWAJS-TAM-LAYOUT    PIC 9(005)   VALUE 24965.

             10 DCOMWAJS-BLOCO-SAIDA.
                15 DCOMWAJS-QTOT-CONTRATOS          PIC  9(010).
                15 DCOMWAJS-QTDE-CONTRATOS          PIC  9(002).
      *-->           --------------------------------------------
      *-->     --->  LISTA DETALHADA DE CONTRATO LIMITE - MAX. 05  <---
      *-->           --------------------------------------------
                15 DCOMWAJS-CONTRATOS               OCCURS 005 TIMES.
                  20 DCOMWAJS-CCONTR-LIM-DESC       PIC  9(009).
                  20 DCOMWAJS-CVRSAO-CONTR-LIM      PIC  9(003).
      *-->           --------------------------------------------
      *-->     --->  DADOS BASICOS DO CONTRATO LIMITE              <---
      *-->           --------------------------------------------
                  20 DCOMWAJS-CSIT-DESC-COML        PIC  9(003).
                  20 DCOMWAJS-IRSUMO-SIT-DESC       PIC  X(015).
                  20 DCOMWAJS-CPRODT-SERVC-OPER     PIC  9(008).
                  20 DCOMWAJS-CPRODT                PIC  9(003).
                  20 DCOMWAJS-IPRODT                PIC  X(060).
                  20 DCOMWAJS-CSPROD-DESC-COML      PIC  9(003).
                  20 DCOMWAJS-IRSUMO-SPROD-DESC     PIC  X(040).
                  20 DCOMWAJS-CCLUB                 PIC  9(010).
                  20 DCOMWAJS-CCNPJ-CPF             PIC  9(009).
                  20 DCOMWAJS-CFLIAL-CNPJ           PIC  9(004).
                  20 DCOMWAJS-CCTRL-CNPJ-CPF        PIC  9(002).
                  20 DCOMWAJS-IPSSOA-DESC-COML      PIC  X(060).
                  20 DCOMWAJS-CINDCD-MODLD-DESC     PIC  X(001).
                  20 DCOMWAJS-CBCO                  PIC  9(003).
                  20 DCOMWAJS-CAG-BCRIA             PIC  9(005).
                  20 DCOMWAJS-CCTA-BCRIA-CLI        PIC  9(013).
                  20 DCOMWAJS-CBCO-OPER-CONTR       PIC  9(003).
                  20 DCOMWAJS-CAG-OPER-CONTR        PIC  9(005).
                  20 DCOMWAJS-DINCL-CONTR-LIM       PIC  X(010).
                  20 DCOMWAJS-DINIC-VGCIA-CONTR     PIC  X(010).
                  20 DCOMWAJS-DVCTO-CONTR-LIM       PIC  X(010).
                  20 DCOMWAJS-TCONTR-LIM-DESC       PIC  9(005).
                  20 DCOMWAJS-CINDCD-PRZ-CONTR      PIC  X(002).
                  20 DCOMWAJS-VCONTR-LIM-DESC       PIC  9(015)V9(002).
                  20 DCOMWAJS-VUTILZ-CONTR-LIM      PIC  9(015)V9(002).
                  20 DCOMWAJS-VDISPN-CONTR-LIM      PIC  9(015)V9(002).
                  20 DCOMWAJS-CINDCD-CONTR-ROTVO    PIC  X(001).
                  20 DCOMWAJS-CINDCD-RENOV-AUTOM    PIC  X(001).
                  20 DCOMWAJS-CINDCD-RTEIO-BNEFC    PIC  X(001).
                  20 DCOMWAJS-CINDCD-LIBRC-CONTR    PIC  X(001).
                  20 DCOMWAJS-CINDCD-TARIF-ADTTO    PIC  X(001).
                  20 DCOMWAJS-DINIC-PARMZ-CONTR     PIC  X(010).
                  20 DCOMWAJS-CTPO-COBR-TAC         PIC  9(003).
                  20 DCOMWAJS-ITPO-COBR-TAC         PIC  X(030).
                  20 DCOMWAJS-CINDCD-TAC-SPROD      PIC  X(001).
                  20 DCOMWAJS-VTAC-CONTR-LIM        PIC  9(015)V9(002).
                  20 DCOMWAJS-DTAC-CONTR-LIM        PIC  X(010).
                  20 DCOMWAJS-DRENOV-CONTR-LIM      PIC  X(010).
                  20 DCOMWAJS-CGARAN                PIC  9(003).
                  20 DCOMWAJS-IGARAN                PIC  X(040).
                  20 DCOMWAJS-RPRCER-FORML-DESC     PIC  X(240).
                  20 DCOMWAJS-FILLER                PIC  X(100).
      *-->           --------------------------------------------
      *-->     --->  LISTA DE CONSULTAS DE LIMITE DE CR�DITO       <---
      *-->           --------------------------------------------
                  20 DCOMWAJS-QTDE-LIMCRED          PIC  9(002).
                  20 DCOMWAJS-LIMCRED               OCCURS 010 TIMES.
CLIM??              25 DCOMWAJS-NDOCTO-PARTC-LIM    PIC  9(010).
CLIM??              25 DCOMWAJS-NSEQ-CONTR-LIM      PIC  9(017).
                    25 DCOMWAJS-DANO-BASE           PIC  9(004).
                    25 DCOMWAJS-CDEPDC              PIC  9(005).
                    25 DCOMWAJS-CSEQ-STUDO          PIC  9(009).
                    25 DCOMWAJS-CCART-LIM-CREDT     PIC  X(005).
                    25 DCOMWAJS-VCONS-NEGOC         PIC  9(015)V9(002).
                    25 DCOMWAJS-FILLER              PIC  X(100).
      *-->          ---------------------------------------------
      *-->    --->  OPTIN - AUTORIZA��O DE D�BITO                  <---
      *-->          ---------------------------------------------
                  20 DCOMWAJS-CINDCD-FORMA-LIQDC    PIC  X(001).
                  20 DCOMWAJS-CINDCD-UTILZ-LIM      PIC  X(001).
                  20 DCOMWAJS-CINDCD-DEB-PCIAL      PIC  X(001).
                  20 DCOMWAJS-CINDCD-TENTV-DEB      PIC  X(001).
      *-->          ---------------------------------------------
      *-->    --->  TESTEMUNHAS DO CONTRATO LIMITE      - MAX. 02  <---
      *-->          ---------------------------------------------
                  20 DCOMWAJS-QTDE-TEST             PIC  9(002).
                  20 DCOMWAJS-TESTEMUNHAS           OCCURS 002 TIMES.
                    25 DCOMWAJS-TEST-CCNPJ-CPF      PIC  9(009).
                    25 DCOMWAJS-TEST-CFLIAL-CNPJ    PIC  9(004).
                    25 DCOMWAJS-TEST-CCTRL-CNPJ-CPF PIC  9(002).
                    25 DCOMWAJS-TEST-CDOCTO-ID      PIC  X(015).
                    25 DCOMWAJS-TEST-IPSSOA-DESC    PIC  X(060).
      *-->          ---------------------------------------------
      *-->    --->  FIADORES DO CONTRATO LIMITE         - MAX. 08  <---
      *-->          ---------------------------------------------
                  20 DCOMWAJS-QTDE-FIAD             PIC  9(002).
                  20 DCOMWAJS-FIADORES              OCCURS 008 TIMES.
                    25 DCOMWAJS-FIAD-TPO-PSSOA      PIC  X(001).
                    25 DCOMWAJS-FIAD-CCNPJ-CPF      PIC  9(009).
                    25 DCOMWAJS-FIAD-CFLIAL-CNPJ    PIC  9(004).
                    25 DCOMWAJS-FIAD-CCTRL-CNPJ-CPF PIC  9(002).
                    25 DCOMWAJS-FIAD-CDOCTO-ID      PIC  X(015).
                    25 DCOMWAJS-FIAD-IPSSOA-DESC    PIC  X(060).
                    25 DCOMWAJS-FIAD-CBCO           PIC  9(003).
                    25 DCOMWAJS-FIAD-CAG-BCRIA      PIC  X(005).
                    25 DCOMWAJS-FIAD-CCTA-BCRIA-CLI
                                                    PIC  9(013).
                    25 DCOMWAJS-FIAD-ELOGDR         PIC  X(040).
                    25 DCOMWAJS-FIAD-IBAIRRO        PIC  X(020).
                    25 DCOMWAJS-FIAD-CID-ELOGDR     PIC  X(040).
                    25 DCOMWAJS-FIAD-CSGL-UF        PIC  X(002).
                    25 DCOMWAJS-FIAD-CCEP           PIC  X(005).
                    25 DCOMWAJS-FIAD-CCEP-COMPL     PIC  X(003).
                    25 DCOMWAJS-CINDCD-CONJG-AUTRZ  PIC  X(001).
                    25 DCOMWAJS-IPSSOA-CONJG        PIC  X(040).
                    25 DCOMWAJS-CCPF-CONJG          PIC  9(009).
                    25 DCOMWAJS-CCTRL-CPF-CONJG     PIC  9(002).
                    25 DCOMWAJS-CDOCTO-ID-CONJG     PIC  X(015).
