      ******************************************************************
      * NOME BOOK : I#DCOMZA                                           *
      * DESCRICAO : BOOK DE INTERFACE PARA CONSULTAR AGENDA DO CLIENTE *
      * DATA      : 08/2020                                            *
      * AUTOR     : ULISSES ASSIS - CAPGEMINI - ALPHA                  *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL
      * TAMANHO   : 0106 BYTES                                         *
      ******************************************************************
      *
       01  DCOMZA-REGISTRO.
           05 DCOMZA-BLOCO-ENTRADA.
              10 DCOMZA-E-CTERM                   PIC  X(008).
              10 DCOMZA-E-CBCO                    PIC  9(003).
              10 DCOMZA-E-CAG-BCRIA               PIC  9(005).
              10 DCOMZA-E-CCTA-BCRIA-CLI          PIC  9(007).
           05 DCOMZA-RETORNO.
              10 DCOMZA-S-COD-RETORNO             PIC  9(004).
              10 DCOMZA-S-MSG-RETORNO             PIC  X(079).
