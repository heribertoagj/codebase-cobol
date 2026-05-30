      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNF *
      * CRIACAO   : 05/2008                                   *
      * DESCRICAO : PFEB - CONSULTA CONVENIOS EXISTENTES NO   *
      *             DCOM PARA 'CLIENTE PAGADOR - CONVENIADA'  *
      * TAMANHO   : LRECL  101                                *
      *********************************************************
       01  NF-REGISTRO.
           05  NF-CCGC-CPF-PGDOR       PIC  9(09) COMP-3.
           05  NF-CFLIAL-CGC-PGDOR     PIC  9(05) COMP-3.
           05  NF-CCTRL-CGC-PGDOR      PIC  9(02) COMP-3.
           05  NF-DT-MIN-VECTO         PIC  9(09) COMP-3.
      *        PREENCHER COM DATA NO FORMATO AAAAMMDD.
           05  NF-MOD-PERMITIDA.
               10  FILLER              OCCURS 15.
                   15  NF-MOD-PGTO     PIC  9(05) COMP-3.
           05  NF-CART-NAO-PERMITIDA.
               10  FILLER              OCCURS 20.
                   15  NF-CART-COBR    PIC  9(03) COMP-3.
           05  NF-OBRIG-NOTA-FISCAL    PIC  X(01).
