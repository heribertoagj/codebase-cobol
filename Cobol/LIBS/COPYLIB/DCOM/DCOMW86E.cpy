      ******************************************************************
      * NOME BOOK : DCOMW86E                                           *
      * DESCRICAO : LAYOUT OPBK PARA STATUS DE OPERACAO                *
      * DATA      : 08/2022                                            *
      * AUTOR     : BRUNO GRACIANO                                     *
      * EMPRESA   : FOURSYS                                            *
      * COMPONENTE: DCOM - OPEN BANK                                   *
      * TAMANHO   : 00198 BYTES                                        *
      ******************************************************************
       01  DCOMW86E-REGISTRO.
           05 DCOMW86E-CD-RECURSO       PIC  X(100).
           05 DCOMW86E-SISTEMA          PIC  X(004).
           05 DCOMW86E-CCONTR           PIC  9(017).
           05 DCOMW86E-CD-IPOC          PIC  X(067).
           05 DCOMW86E-DT-FIM           PIC  X(010).
