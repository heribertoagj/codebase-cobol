      ******************************************************************
      * DESCRICAO  : INCLUIR CONTRATO LIMITE PARA JORNADA DA AUTOMACAO *
      *              DA DA CONTRATACAO DO CONTRATO LIMITE              *
      * COPYBOOK   : DCOMWACE - BOOK DE ENTRADA                        *
      * COORDENADOR: DCOM1ACI - INCLUSAO CONTRATO LIMITE               *
      * FLUXO      : DCOMIAAU                                          *
      * DATA       : 15/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWACE-CTERM        = IDENTIFICACAO DO CANAL                 *
      * DCOMWACE-CTPO-DESC-COML                                        *
      *                       = TIPO DE DESCONTO COMERCIAL             *
      *                          [004 = DESCONTO DE ORPAG]             *
      * DCOMWACE-CBCO         = CODIGO DO BANCO [237 - BRADESCO]       *
      * DCOMWACE-CAG-BCRIA    = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWACE-CCTA-BCRIA-CLI                                        *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      * DCOMWACE-CCNPJ-CPF    = NUMERO DO CNPJ/CPF DO CLIENTE          *
      * DCOMWACE-CFLIAL-CNPJ  = NUMERO DA FILIAL DO CNPJ DO CLIENTE    *
      *                         INFORMAR ZEROS EM CASO DE CPF          *
      * DCOMWACE-CINDCD-FORMA-LIQDC                                    *
      *                       = IDENTIFICA A FORMA DE LIQUIDEZ         *
      *                          ["D"= DEBITO EM CONTA / "B"= BOLETO]  *
      * DCOMWACE-CINDCD-UTILZ-LIMDC                                    *
      *                       = IDENTIFICA A UTILIZACAO LIMITE CREDITO *
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWACE-CINDCD-DEB-PCIALDC                                    *
      *                       = IDENTIFICA DEBITO PARCIAL              *
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWACE-CINDCD-TENTV-DEB                                      *
      *                       = IDENTIFICA TENTATIVA DEBITO(TEIMOSINHA)*
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWACE-DANO-BASE    = CONSULTA DE CREDITO(ANO BASE CONE/CLIM)*
      * DCOMWACE-CDEPDC       = CONSULTA DE CREDITO(AGENCIA CONE/CLIM) *
      * DCOMWACE-CSEQ-STUDO   = CONSULTA DE CREDITO(SEQUENCIA CONE/CLIM*
      * DCOMWACE-CCART-LIM-CREDT                                       *
      *                       = CONSULTA DE CREDITO(CARTEIRA CONE/CLIM *
      * DCOMWACE-VCONS-NEGOC  = CONSULTA DE CREDITO(VL CONSUL CONE/CLIM*
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWACE-HEADER.
             10 DCOMWACE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWACE'.
             10 DCOMWACE-TAM-LAYOUT    PIC 9(005) VALUE 00200.
      *
          05 DCOMWACE-REGISTRO.
             10 DCOMWACE-BLOCO-ENTRADA.
                15 DCOMWACE-CTERM                 PIC  X(008).
                15 DCOMWACE-CTPO-DESC-COML        PIC  9(003).
                15 DCOMWACE-CBCO                  PIC  9(003).
                15 DCOMWACE-CAG-BCRIA             PIC  9(005).
                15 DCOMWACE-CCTA-BCRIA-CLI        PIC  9(013).
                15 DCOMWACE-CCNPJ-CPF             PIC  9(009).
                15 DCOMWACE-CFLIAL-CNPJ           PIC  9(004).
                15 DCOMWACE-CCTRL-CNPJ-CPF        PIC  9(002).
                15 DCOMWACE-CINDCD-FORMA-LIQDC    PIC  X(001).
                15 DCOMWACE-CINDCD-UTILZ-LIM      PIC  X(001).
                15 DCOMWACE-CINDCD-DEB-PCIAL      PIC  X(001).
                15 DCOMWACE-CINDCD-TENTV-DEB      PIC  X(001).
                15 DCOMWACE-DANO-BASE             PIC  9(004).
                15 DCOMWACE-CDEPDC                PIC  9(005).
                15 DCOMWACE-CSEQ-STUDO            PIC  9(009).
                15 DCOMWACE-CCART-LIM-CREDT       PIC  X(005).
                15 DCOMWACE-VCONS-NEGOC           PIC  9(016)V9(002).
                15 DCOMWACE-RESERVA               PIC  X(095).
