      *----------------------------------------------------------------*
      *  NOME INC                                       LENGTH 588     *
      *  I#DCOMXC                                       SISTEMA DCOM   *
      *----------------------------------------------------------------*
      *                                                                *
      *  DESCRICAO: CONSULTA DE RECEBIVEIS - BVVE1310                  *
      *             OBTER DESCRICAO DOS CARTOES DE CREDITO             *
      *----------------------------------------------------------------*

       01  BVVE1310-DADOS.

           05  BVVE-ENTRADA.

      *-----    LISTA(L)  OU  NICO(U)                          001/001 *
             10  BVVE-LISTA-UNICO-E            PIC X(001).

      *-----    ROTATIVO(R) OU  PARCELADO(P)                   002/002 *
             10  BVVE-ROT-PARCELADO-E          PIC X(001).

      *-----    CODIGO DO PRODUTO                              003/004 *
             10  BVVE-COD-PRODUTO-E            PIC 9(002).

           05  BVVE-RETORNO.

      *-----    CODIGO DE RETORNO                              005/006 *
             10  BVVE-COD-RETORNO-S            PIC 9(002).
      *         DOMINIO:
      *            00 - EXECUCAO NORMAL
      *            01 - EXECUCAO NORMAL COM RESTART
      *            02 - INCONSISTENCIA NA AREA DE ENTRADA
      *            03 - ARQUMENTO DE PESQUISA INEXISTENTE
      *            04 - REGISTRO INEXISTENTE
      *            05 - SISTEMA INDISPONIVEL - TABELAS EM ATUALIZACAO
      *            88 - ERRO GRAVE NO PROCESSAMENTO

      *-----    LISTA(L)  OU  NICO(U)                          007/007 *
             10  BVVE-LISTA-UNICO-S            PIC X(001).

      *-----    ROTATIVO(R) OU  PARCELADO(P)                   008/008 *
             10  BVVE-ROT-PARCEL-S             PIC X(001).

      *-----    CODIGO DO PRODUTO                              009/010 *
             10  BVVE-COD-PROD-S               PIC 9(002).

      *-----    QUANTIDADE DE OCORRENCIAS                      011/000 *
             10  BVVE-OCORRENCIAS-S OCCURS 10 TIMES.

      *-----    CODIGO DO PRODUTO                              011/012 *
               15 BVVE-COD-PRODUTO-S           PIC 9(002).

      *-----    ROTATIVO(R) OU  PARCELADO(P)                   013/013 *
               15 BVVE-ROT-PARCELADO-S         PIC X(001).

      *-----    DESCRICAO DO PRODUTO                           014/038 *
               15 BVVE-DESC-PRODUTO-S          PIC X(025).

      *-----    CODIGO DE ERRO - SQLCODE                       291/294*
             10  BVVE-SQLCODE                  PIC S9(04).

      *-----    MENSAGEM DE RETORNO                            295/345*
             10  BVVE-MENSAGEM                 PIC X(050).

      *-----    AREA DE ERRO DO DB2                            346/452*
             10  BVVE-ERRO-DB2                 PIC X(107).

      *-----    AREA DE ERRO DO DB2                            453/588*
             10  BVVE-SQLCA                    PIC X(136).

