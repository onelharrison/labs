import numpy as np
from sklearn.datasets import make_blobs

# TODO: Visualize clusters using matplotlib


class KMeans:
    def __init__(
        self,
        num_clusters=2,
        max_iterations=100,
        random_centroid_initialization=True,
    ):
        self.num_clusters = num_clusters
        self.max_iterations = max_iterations
        self.random_centroid_initialization = random_centroid_initialization

    def _initialize_centroids(self, X):
        num_observations, num_features = X.shape
        centroids = np.zeros((self.num_clusters, num_features))
        for centroid_index in range(self.num_clusters):
            if self.random_centroid_initialization:
                centroids[centroid_index] = X[np.random.choice(num_observations)]
            else:  # use first n observations as initial centroids
                centroids[centroid_index] = X[centroid_index]
        return centroids

    def _create_clusters(self, centroids, X):
        clusters = [[]] * self.num_clusters
        # TODO: Vectorize
        for observation_index, observation in enumerate(X):
            cluster_index = np.argmin(
                [np.linalg.norm(observation - centroid) for centroid in centroids]
            )
            clusters[cluster_index].append(observation_index)
        return clusters

    def _calculate_cluster_centroids(self, clusters, X):
        _, num_features = X.shape
        centroids = np.zeros((self.num_clusters, num_features))
        for cluster_index, cluster in enumerate(clusters):
            centroids[cluster_index] = np.mean(cluster, axis=0)
        return centroids

    def _centroids_converged(self, previous_centroids, current_centroids):
        return (
            np.sum(
                [
                    np.linalg.norm(previous_centroids[i] - current_centroids[i])
                    for i in range(self.num_clusters)
                ]
            )
            == 0
        )

    def fit(self, X):
        current_centroids = self._initialize_centroids(X)

        for it in range(self.max_iterations):
            clusters = self._create_clusters(current_centroids, X)
            previous_centroids = current_centroids
            current_centroids = self._calculate_cluster_centroids(clusters, X)
            if self._centroids_converged(previous_centroids, current_centroids):
                print(f"K-means converged at iteration {it} of {self.max_iterations}")
                break

        return clusters


if __name__ == "__main__":
    np.random.seed(42)
    num_clusters = 3
    kmeans = KMeans(num_clusters=num_clusters)
    X, _ = make_blobs(n_samples=10000, n_features=5, centers=num_clusters)
    kmeans.fit(X)
