      ******************************************************************
      * NOME BOOK : DCOMWRTC - BOOK DE INTERFACE COM PROGRAMA DCOM3RTC *
      * DESCRICAO : AREA DE COMUNICACAO PROGRAMA ROTEADOR CISC X IMS   *
      * DATA      : 15/02/2024                                         *
      * EMPRESA   : CAPGEMINI                                          *
      * ANALISTA  : EDGARD ALMEIDA                                     *
      * COMPONENTE: DCOM                                               *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
      *
           05  DCOMWRTC-HEADER.
               10 DCOMWRTC-E-COD-LAYOUT PIC  X(008)  VALUE 'DCOMWRTC'.
               10 DCOMWRTC-E-TAM-LAYOUT PIC  9(005)  VALUE 03916.
      *
           05  DCOMWRTC-REGISTRO.
               10 DCOMWRTC-FUNCAO       PIC  9(003).
               10 DCOMWRTC-DADOS        PIC  X(3900).
