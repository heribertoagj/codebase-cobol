      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY2 *
      * CRIACAO   : 09/2006                                   *
      * DESCRICAO : ACESSO MODULO IRES2565 (RATING)           *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM6414                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-2565.
           03 WRK-ENVIO-2565.
              05 WRK-CCNPJ-CPF-2565             PIC  9(09).
              05 WRK-CFLIAL-CNPJ-2565           PIC  9(05).
              05 WRK-TIPO-ACESSO-2565           PIC  9(01).
              05 WRK-QTDE-HIST-2565             PIC  9(02).
              05 WRK-ACUM-HIST-2565             PIC  9(03).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************


           03 WRK-RETORNO-2565.
              05 WRK-COD-RET-2565               PIC  9(01).
              05 WRK-SAI-ACUM-HIST-2565         PIC  9(03).
              05 WRK-FIM-PESQ-2565              PIC  X(01).
              05 WRK-OCORR-RATING-2565 OCCURS 25 TIMES.
                 07  WRK-TIPO-RATING-2565       PIC  X(02).
                 07  WRK-COD-RATING-2565        PIC  X(03).
                 07  WRK-REFERENCIA-2565        PIC  X(10).
                 07  WRK-DATA-RATING-2565       PIC  9(08).
                 07  WRK-DESC-RATING-2565       PIC  X(15).

