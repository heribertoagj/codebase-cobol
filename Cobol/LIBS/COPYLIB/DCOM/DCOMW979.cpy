      ******************************************************************
      * NOME BOOK : DCOMW979 - INTERFACE DO COORDENADOR E FUNCIONAL    *
      * DESCRICAO : DCOM - DESCONTO COMERCIAL                          *
      * DATA      : 15/04/2015                                         *
      * AUTOR     : JOSE ROBERTO SANTOS DE SOUSA                       *
      * EMPRESA   : CDI - CENTRO DE DESENVOLVIMENTO INTERNO            *
      * GRUPO     : BRADESCO - DDS                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 420 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      *                                                                *
      * DCOMW979-E-BANCO...: CODIGO DO BANCO                           *
      * DCOMW979-E-AGENCIA.: NUMERO DA AGENCIA                         *
      * DCOMW979-E-CONTA...: NUMERO DA CONTA                           *
      * DCOMW979-E-DATA....: DATA DD-MM-AAA                            *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW979-S-ORPAG...: ORPAG                                     *
      *********************** DADOS DE RETORNO *************************
      * DCOMW979-COD-RET...: CODIGO DE RETORNO                         *
      * DCOMW979-COD-ERRO..: CODIGO DE ERRO                            *
      * DCOMW979-MSG-RET...: MENSAGEM DE RETORNO                       *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 DCOMW979E-HEADER.
             10 DCOMW979-COD-LAYOUT    PIC X(08)   VALUE 'DCOMW979'.
             10 DCOMW979-TAM-LAYOUT    PIC 9(05)   VALUE 00091.
           05 DCOMW979-REGISTRO.
             10 DCOMW979-BLOCO-ENTRADA.
               15 DCOMW979-E-BANCO                 PIC 9(003).
               15 DCOMW979-E-AGENCIA               PIC 9(005).
               15 DCOMW979-E-CONTA                 PIC 9(013).
               15 DCOMW979-E-DATA                  PIC X(010).
             10 DCOMW979-BLOCO-SAIDA.
               15 DCOMW979-S-ORPAG                 PIC X(001).
             10 DCOMW979-BLOCO-RETORNO.
               15  DCOMW979-COD-RET                PIC 9(002).
               15  DCOMW979-COD-ERRO               PIC X(004).
               15  DCOMW979-MSG-RET                PIC X(040).
