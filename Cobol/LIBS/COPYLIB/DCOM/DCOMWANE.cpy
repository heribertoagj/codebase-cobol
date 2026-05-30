      *================================================================*
      * DESCRICAO  : GERAR CONTRATO ARQUITETURA - BOTAO CONTRATAR      *
      * COPYBOOK   : DCOMWANE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ANI - GERAR CONTRATO ARQUITETURA             *
      * FLUXO      : DCOMIABM                                          *
      * DATA       : 28/10/2024                                        *
      * AUTOR      : FABIO AUGUSTO FINK - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWANE-E-HEADER.
             10 DCOMWANE-COD-LAYOUT    PIC X(008)  VALUE 'DCOMWANE'.
             10 DCOMWANE-TAM-LAYOUT    PIC 9(005)  VALUE 00500.
      *
          05 DCOMWANE-BLOCO-ENTRADA.
      *-->   DADOS DA PROPOSTA/SIMULACAO/CLIENTE
      *-->   =========================================
             10 DCOMWANE-CPPSTA-PRODT              PIC  9(019).
             10 DCOMWANE-NSMULA-OPER-DESC          PIC  9(009).
             10 DCOMWANE-CAG-BCRIA                 PIC  9(005).
             10 DCOMWANE-CCTA-BCRIA-CLI            PIC  9(013).
             10 DCOMWANE-CCNPJ-CPF                 PIC  9(009).
             10 DCOMWANE-CFLIAL-CNPJ               PIC  9(004).
             10 DCOMWANE-CCTRL-CNPJ-CPF            PIC  9(002).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
             10 DCOMWANE-FILLER                    PIC  X(426).
      *
